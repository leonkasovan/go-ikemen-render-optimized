package main

import (
	"bytes"
	"container/list"
	_ "embed"
	"encoding/binary"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/png"
	"io"
	"log"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"unsafe"

	mgl "github.com/go-gl/mathgl/mgl32"
	"github.com/ikemen-engine/Ikemen-GO/packages/gl/v3.3-core/gl"
	"github.com/ikemen-engine/Ikemen-GO/packages/glfw"
	"golang.org/x/mobile/exp/f32"
)

const (
	scr_width  = 640
	scr_height = 480
	IMax       = int32(^uint32(0) >> 1)
	IErr       = ^IMax
)

var sys_gameWidth = 320
var sys_gameHeight = 200
var sys_widthScale = float32(1.0)
var sys_heightScale = float32(1.0)
var sys_brightness = float32(1.0)

type TextureSamplingParam int

const (
	TextureSamplingFilterNearest = TextureSamplingParam(iota)
	TextureSamplingFilterLinear
	TextureSamplingFilterNearestMipMapNearest
	TextureSamplingFilterLinearMipMapNearest
	TextureSamplingFilterNearestMipMapLinear
	TextureSamplingFilterLinearMipMapLinear
	TextureSamplingWrapClampToEdge
	TextureSamplingWrapMirroredRepeat
	TextureSamplingWrapRepeat
)

type Texture interface {
	SetData(data []byte)
	SetSubData(data []byte, x, y, width, height int32)
	SetDataG(data []byte, mag, min, ws, wt TextureSamplingParam)
	SetPixelData(data []float32)
	IsValid() bool
	GetWidth() int32
	GetHeight() int32
	CopyData(src *Texture)
	SavePNG(filename string, pal []uint32) error
}

// Tiling holds tiling parameters
type Tiling struct {
	xflag, yflag       int32
	xspacing, yspacing int32
}

var notiling = Tiling{}

// Rotation holds rotation parameters
type Rotation struct {
	angle, xangle, yangle float32
}
type PrimitiveMode byte

const (
	POINTS = iota
	LINES
	LINE_LOOP
	LINE_STRIP
	TRIANGLES
	TRIANGLE_STRIP
	TRIANGLE_FAN
)

type TransType int32

const (
	TT_none TransType = iota
	TT_add
	TT_sub
	TT_default
)

type PalFXDef struct {
	time        int32
	color       float32
	add         [3]int32
	mul         [3]int32
	sinadd      [3]int32
	sinmul      [3]int32
	sincolor    int32
	sinhue      int32
	cycletime   [4]int32
	invertall   bool
	invertblend int32
	hue         float32
	interpolate bool
	iadd        [6]int32
	imul        [6]int32
	icolor      [2]float32
	ihue        [2]float32
	itime       int32
}

type PalFX struct {
	PalFXDef
	remap        []int
	allowNeg     bool // Can use negative color math
	sintime      [4]int32
	enable       bool
	eAllowNeg    bool
	eInvertall   bool
	eInvertblend int32
	eAdd         [3]int32
	eMul         [3]int32
	eColor       float32
	eHue         float32
	eInterpolate bool
	eiAdd        [3]int32
	eiMul        [3]int32
	eiColor      float32
	eiHue        float32
	eiTime       int32
}

func newPalFX() *PalFX {
	return &PalFX{}
}

// RenderParams holds the common data for all sprite rendering functions
type RenderParams struct {
	tex            Texture // Sprite
	paltex         Texture // Palette
	size           [2]uint16
	x, y           float32 // Position
	tile           Tiling
	xts, xbs       float32 // Top and bottom X scale (as in parallax)
	ys, vs         float32 // Y scale
	rxadd          float32
	xas, yas       float32
	rot            Rotation
	tint           uint32 // Sprite tint for shadows
	blendMode      TransType
	blendAlpha     [2]int32
	mask           int32 // Mask for transparency
	pfx            *PalFX
	window         *[4]int32
	rcx, rcy       float32 // Rotation center
	projectionMode int32   // Perspective projection
	fLength        float32 // Focal length
	xOffset        float32
	yOffset        float32
	uv             [4]float32 // Optional atlas UV rect u1,v1,u2,v2
	projection     mgl.Mat4
}

type BlendFunc int

const (
	BlendOne = BlendFunc(iota)
	BlendZero
	BlendSrcAlpha
	BlendOneMinusSrcAlpha
	BlendDstColor
	BlendOneMinusDstColor
)

type BlendEquation int

const (
	BlendAdd = BlendEquation(iota)
	BlendReverseSubtract
)

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// SFF / Sprite types (moved here to keep all type declarations grouped)

// SffHeader represents the header of an SFF file
type SffHeader struct {
	Ver0, Ver1, Ver2, Ver3   byte
	FirstSpriteHeaderOffset  uint32
	FirstPaletteHeaderOffset uint32
	NumberOfSprites          uint32
	NumberOfPalettes         uint32
}

// PaletteList stores palettes used by SFFs
type PaletteList struct {
	palettes   [][]uint32
	paletteMap []int
	PalTable   map[[2]uint16]int
	numcols    map[[2]uint16]int
	PalTex     []Texture
}

// Sff is a lightweight representation of a loaded SFF file
type Sff struct {
	header  SffHeader
	sprites map[[2]uint16]*Sprite
	palList PaletteList
	// This is the sffCache key
	filename string
	Atlas_8  *TextureAtlas // 8-bit atlas for paletted sprites
}

type Palette struct {
	palList PaletteList
}

// Sprite stores texture and palette info for a single sprite
type Sprite struct {
	Pal      []uint32
	Tex      Texture
	UV       [4]float32 // u1,v1,u2,v2
	Group    uint16     // Group index: valid range 0–65535
	Number   uint16     // Sprite index: valid range 0–65535
	Size     [2]uint16
	Offset   [2]int16
	palidx   int
	rle      int
	coldepth byte
	paltemp  []uint32
	PalTex   Texture
	Sff      *Sff // Reference to parent SFF
}

// ------------------------------------------------------------------
// Font types

// FntCharImage stores sprite and position
type FntCharImage struct {
	ofs, w uint16
	img    []Sprite
}

// Fnt is a structure for basic font information
type Fnt struct {
	images    map[int32]map[rune]*FntCharImage
	palettes  [][256]uint32
	coldepth  []byte
	ver, ver2 uint16
	Type      string
	BankType  string
	Size      [2]uint16
	Spacing   [2]int32
	colors    int32
	offset    [2]int32
	paltex    Texture
}

type glyphDrawInfo struct {
	x    float32
	y    float32
	xscl float32
	yscl float32
	c    rune
	bank int32
	bt   int32
	pal  []uint32
}

// A simple SFF cache storing shallow copies
type SffCacheEntry struct {
	sffData  Sff
	refCount int
}

var SffCache = map[string]*SffCacheEntry{}

// PaletteList initializer
func (pl *PaletteList) init() {
	pl.palettes = nil
	pl.paletteMap = nil
	pl.PalTable = make(map[[2]uint16]int)
	pl.numcols = make(map[[2]uint16]int)
	pl.PalTex = nil
}

// newSff constructs a new, empty Sff structure with default palette entries
func newSff() (s *Sff) {
	s = &Sff{sprites: make(map[[2]uint16]*Sprite)}
	s.palList.init()
	for i := uint16(1); i <= uint16(sys_cfg_Config_PaletteMax); i++ {
		s.palList.PalTable[[...]uint16{1, i}], _ = s.palList.NewPal()
	}
	s.Atlas_8 = nil
	return
}

// ------------------------------------------------------------------
// Texture / Rendering types

type TextureAtlas struct {
	texture Texture
	width   int32
	height  int32
	depth   int32
	filter  bool
	resize  bool
	skyline *list.List //[][2]uint32
}

// ------------------------------------------------------------------
// ShaderProgram_GL

// (ShaderProgram_GL type moved to top types section)

// ------------------------------------------------------------------
// Texture_GL

// (Texture_GL type moved to top types section)

// ------------------------------------------------------------------
// Renderer_GL and GLState

type GLState struct {
	depthTest           bool
	depthMask           bool
	invertFrontFace     bool
	doubleSided         bool
	blendEquation       BlendEquation
	blendSrc            BlendFunc
	blendDst            BlendFunc
	useUV               bool
	useNormal           bool
	useTangent          bool
	useVertColor        bool
	useJoint0           bool
	useJoint1           bool
	useOutlineAttribute bool
}

// Shader program, texture and renderer types
type ShaderProgram_GL struct {
	program uint32
	a       map[string]int32
	u       map[string]int32
	t       map[string]int
}

type Texture_GL struct {
	width  int32
	height int32
	depth  int32
	filter bool
	handle uint32
}

type Renderer_GL struct {
	fbo         uint32
	fbo_texture uint32
	// Normal rendering
	rbo_depth uint32
	// MSAA rendering
	fbo_f         uint32
	fbo_f_texture *Texture_GL
	// Shadow Map
	fbo_shadow              uint32
	fbo_shadow_cube_texture uint32
	fbo_env                 uint32
	// Postprocessing FBOs
	fbo_pp         []uint32
	fbo_pp_texture []uint32
	// Post-processing shaders
	postVertBuffer   uint32
	postShaderSelect []*ShaderProgram_GL
	// Shader and vertex data for primitive rendering
	spriteShader *ShaderProgram_GL
	vertexBuffer uint32
	// Shader and index data for 3D model rendering
	shadowMapShader         *ShaderProgram_GL
	modelShader             *ShaderProgram_GL
	panoramaToCubeMapShader *ShaderProgram_GL
	cubemapFilteringShader  *ShaderProgram_GL
	modelVertexBuffer       [2]uint32
	modelIndexBuffer        [2]uint32
	vao                     uint32
	enableModel             bool
	enableShadow            bool
	GLState
}

// ------------------------------------------------------------------
// Utilities (helpers & small helpers)

// Global Variable
var sys_scrrect = [...]int32{0, 0, scr_width, scr_height}
var sys_allPalFX = newPalFX()
var gfx = &Renderer_GL{}
var sys_mainThreadTask = make(chan func(), 65536)
var sys_nDrawcall = 0
var sys_Drawcall = 0

// for avg. FPS calculations
var sys_gameFPS = float64(0.0)
var sys_prevTimestamp = float64(0.0)
var sys_absTickCountF = float64(0.0)

// Add to global variables
var textVertexBuffer uint32
var textVertexData []float32

// Initialize in gfx.Init() or similar
func initTextBuffers() {
	gl.GenBuffers(1, &textVertexBuffer)
	textVertexData = make([]float32, 0, 1024) // Pre-allocate space for vertices
}

func updateFPS() {
	currentTime := sys_GetTime()
	deltaTime := currentTime - sys_prevTimestamp

	if deltaTime >= 1 {
		sys_gameFPS = sys_absTickCountF / deltaTime / 2
		sys_absTickCountF = 0
		sys_prevTimestamp = currentTime
	}

	sys_absTickCountF++
}

func sys_GetTime() float64 {
	return glfw.GetTime()
}
func IsFinite(f float32) bool {
	return math.Abs(float64(f)) <= math.MaxFloat64
}
func Min(arg ...int32) (min int32) {
	for i, x := range arg {
		if i == 0 || x < min {
			min = x
		}
	}
	return
}

func Max(arg ...int32) (max int32) {
	for i, x := range arg {
		if i == 0 || x > max {
			max = x
		}
	}
	return
}
func Pow(x, y float32) float32 {
	return float32(math.Pow(float64(x), float64(y)))
}
func Clamp(x, a, b int32) int32 {
	return Max(a, Min(x, b))
}
func Btoi(b bool) int32 {
	if b {
		return 1
	}
	return 0
}
func MaxF(arg ...float32) (max float32) {
	for i, x := range arg {
		if i == 0 || x > max {
			max = x
		}
	}
	return
}
func AbsF(f float32) float32 {
	if f < 0 {
		return -f
	}
	return f
}

// Checks if error is not null, if there is an error it displays a error dialogue box and crashes the program.
func chk(err error) {
	if err != nil {
		fmt.Println(err.Error())
		panic(err)
	}
}

// Extended version of 'chk()'
func chkEX(err error, txt string, crash bool) bool {
	if err != nil {
		fmt.Println(txt + err.Error())
		if crash {
			panic(err)
		}
		return true
	}
	return false
}

type Error string

func (e Error) Error() string {
	return string(e)
}
func (r *Rotation) IsZero() bool {
	return r.angle == 0 && r.xangle == 0 && r.yangle == 0
}

func (rp *RenderParams) IsValid() bool {
	return rp.tex != nil && rp.tex.IsValid() && rp.size[0] != 0 && rp.size[1] != 0 &&
		IsFinite(rp.x+rp.y+rp.xts+rp.xbs+rp.ys+rp.vs+rp.rxadd+rp.rot.angle+rp.rcx+rp.rcy)
}

// ------------------------------------------------------------------
// Initialization helpers (GLFW / OpenGL / main-thread utils)

func init() {
	runtime.LockOSThread()
}
func sys_runMainThreadTask() {
	for {
		select {
		case f := <-sys_mainThreadTask:
			f()
		default:
			return
		}
	}
}

func GetScaledViewportSize() (int32, int32, int32, int32) {
	return 0, 0, scr_width, scr_height
}

func initGLFW() *glfw.Window {
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}

	glfw.WindowHint(glfw.ContextVersionMajor, 3)
	glfw.WindowHint(glfw.ContextVersionMinor, 3)
	glfw.WindowHint(glfw.OpenGLProfile, glfw.OpenGLCoreProfile)
	glfw.WindowHint(glfw.OpenGLForwardCompatible, glfw.True)

	window, err := glfw.CreateWindow(scr_width, scr_height, "Colored Rectangle", nil, nil)
	if err != nil {
		log.Fatalln("failed to create window:", err)
	}
	window.MakeContextCurrent()

	return window
}

func initOpenGL() {
	if err := gl.Init(); err != nil {
		log.Fatalln("failed to initialize OpenGL:", err)
	}
	version := gl.GoStr(gl.GetString(gl.VERSION))
	fmt.Println("OpenGL version:", version)
}

// NOTE: Per-file method Renderer_GL.compileShader is used for shader compilation.
// The global compileShader helper was removed because it wasn't referenced.

func initRenderSpriteQuad(rp *RenderParams) {
	if rp.vs < 0 {
		rp.vs *= -1
		rp.ys *= -1
		rp.rot.angle *= -1
		rp.rot.xangle *= -1
	}
	if rp.tile.xflag == 0 {
		rp.tile.xspacing = 0
	} else if rp.tile.xspacing > 0 {
		rp.tile.xspacing -= int32(rp.size[0])
	}
	if rp.tile.yflag == 0 {
		rp.tile.yspacing = 0
	} else if rp.tile.yspacing > 0 {
		rp.tile.yspacing -= int32(rp.size[1])
	}
	if rp.xts >= 0 {
		rp.x *= -1
	}
	rp.x += rp.rcx
	rp.rcy *= -1
	if rp.ys < 0 {
		rp.y *= -1
	}
	rp.y += rp.rcy
}

func newPalFXDef() *PalFXDef {
	return &PalFXDef{
		color:  1,
		icolor: [...]float32{1, 1},
		mul:    [...]int32{256, 256, 256},
		imul:   [...]int32{256, 256, 256, 256, 256, 256},
	}
}
func (pf *PalFX) clearWithNeg(neg bool) {
	pf.PalFXDef = *newPalFXDef()
	pf.allowNeg = neg
	for i := 0; i < len(pf.sintime); i++ {
		pf.sintime[i] = 0
	}
}
func (pf *PalFX) clear() {
	pf.clearWithNeg(false)
}
func Abs(i int32) int32 {
	if i < 0 {
		return -i
	}
	return i
}
func (pf *PalFX) synthesize(pfx *PalFX, alpha [2]int32) {
	if alpha[0] == -2 { // Sub magic number
		for i, a := range pfx.eAdd {
			pf.eAdd[i] = Clamp(pf.eAdd[i]-Abs(a), 0, 255)
			pf.eMul[i] = Clamp(pf.eMul[i]-a, 0, 255)
		}
	} else {
		for i, a := range pfx.eAdd {
			pf.eAdd[i] += a
		}
	}
	for i, m := range pfx.eMul {
		pf.eMul[i] = pf.eMul[i] * m / 256
	}

	pf.eHue += pfx.eHue
	pf.eColor *= pfx.eColor
	pf.eInvertall = pf.eInvertall != pfx.eInvertall

	if pfx.invertall {
		// Char blend inverse
		if pfx.invertblend == 1 {
			if alpha != [2]int32{0, 0} && pf.invertblend > -3 {
				pf.eInvertall = pf.invertall
			}
			switch {
			case pf.invertblend == 0:
				pf.eInvertblend = 1
			case pf.invertblend == 1:
				pf.eInvertblend = 0
			case pf.invertblend == -2:
				if pf.eInvertall {
					pf.eInvertall = false
					pf.eInvertblend = -2
				} else {
					pf.eInvertblend = 3
				}
			case pf.invertblend == 2:
				pf.eInvertall = pf.invertall
				pf.eInvertblend = -1
			case pf.invertblend == -1:
				pf.eInvertall = pf.invertall
				pf.eInvertblend = 2
			}
		}

		// Bg blend inverse
		if pf.invertblend == -3 {
			if pf.eInvertall {
				pf.eInvertblend = 3
			} else {
				pf.eInvertall = false
				pf.eInvertblend = -3
			}
		}
	}

}
func (pf *PalFX) getSynFx(alpha [2]int32) *PalFX {
	if pf == nil || !pf.enable {
		if alpha[0] == -2 && sys_allPalFX.enable { // Sub magic number
			if pf == nil {
				pf = newPalFX()
			}
			pf.clear()
			pf.enable = true
			pf.eMul = pf.mul
			pf.eAdd = pf.add
			pf.eColor = pf.color
			pf.eHue = pf.hue
		} else {
			return sys_allPalFX
		}
	}
	if !sys_allPalFX.enable {
		return pf
	}
	synth := *pf
	synth.synthesize(sys_allPalFX, alpha)
	return &synth
}

func (pf *PalFX) getFcPalFx(transNeg bool, alpha [2]int32) (neg bool, grayscale float32,
	add, mul [3]float32, invblend int32, hue float32) {
	p := pf.getSynFx(alpha)
	if !p.enable {
		neg = false
		grayscale = 0
		for i := range add {
			add[i] = 0
		}
		for i := range mul {
			mul[i] = 1
		}
		return
	}
	neg = p.eInvertall
	grayscale = 1 - p.eColor
	invblend = p.eInvertblend
	hue = -(p.eHue * 180.0) * (math.Pi / 180.0)
	if !p.eAllowNeg {
		transNeg = false
	}
	for i, v := range p.eAdd {
		add[i] = float32(v) / 255
		if transNeg {
			//add[i] *= -1
			mul[i] = float32(p.eMul[(i+1)%3]+p.eMul[(i+2)%3]) / 512
		} else {
			mul[i] = float32(p.eMul[i]) / 256
		}
	}
	return
}

// Builds the base projection transform depending on projectionMode
func applyProjection(modelview mgl.Mat4, rp RenderParams, n int, botdist, dy float32) mgl.Mat4 {
	if rp.projectionMode == 0 {
		// No projection, just center on pivot + tile offset
		return modelview.Mul4(mgl.Translate3D(rp.rcx+float32(n)*botdist, rp.rcy+dy, 0))
	}

	matrix := mgl.Mat4{float32(sys_scrrect[2] / 2.0), 0, 0, 0, 0, float32(sys_scrrect[3] / 2), 0, 0, 0, 0, -65535, 0, float32(sys_scrrect[2] / 2), float32(sys_scrrect[3] / 2), 0, 1}

	if rp.projectionMode == 1 {
		modelview = modelview.Mul4(mgl.Translate3D(0, -float32(sys_scrrect[3]), rp.fLength))
		modelview = modelview.Mul4(matrix)
		modelview = modelview.Mul4(mgl.Frustum(-float32(sys_scrrect[2])/2/rp.fLength, float32(sys_scrrect[2])/2/rp.fLength, -float32(sys_scrrect[3])/2/rp.fLength, float32(sys_scrrect[3])/2/rp.fLength, 1.0, 65535))
		modelview = modelview.Mul4(mgl.Translate3D(-float32(sys_scrrect[2])/2.0, float32(sys_scrrect[3])/2.0, -rp.fLength))
		return modelview.Mul4(mgl.Translate3D(rp.rcx+float32(n)*botdist, rp.rcy+dy, 0))
	}

	if rp.projectionMode == 2 {
		modelview = modelview.Mul4(mgl.Translate3D(rp.rcx-float32(sys_scrrect[2])/2.0-rp.xOffset, rp.rcy-float32(sys_scrrect[3])/2.0+rp.yOffset, rp.fLength))
		modelview = modelview.Mul4(matrix)
		modelview = modelview.Mul4(mgl.Frustum(-float32(sys_scrrect[2])/2/rp.fLength, float32(sys_scrrect[2])/2/rp.fLength, -float32(sys_scrrect[3])/2/rp.fLength, float32(sys_scrrect[3])/2/rp.fLength, 1.0, 65535))
		return modelview.Mul4(mgl.Translate3D(rp.xOffset+float32(n)*botdist, -rp.yOffset+dy, -rp.fLength))
	}

	return modelview
}

func sys_getCurrentAspect() float32 {
	return 4.0 / 3.0
}

func applyRotation(modelview mgl.Mat4, rp RenderParams) mgl.Mat4 {
	aspectGame := sys_getCurrentAspect()
	aspectWindow := float32(sys_scrrect[2]) / float32(sys_scrrect[3])

	rotMatrix := func() mgl.Mat4 {
		return mgl.Rotate3DX(-rp.rot.xangle * math.Pi / 180.0).
			Mul3(mgl.Rotate3DY(rp.rot.yangle * math.Pi / 180.0)).
			Mul3(mgl.Rotate3DZ(rp.rot.angle * math.Pi / 180.0)).
			Mat4()
	}

	if AbsF(aspectGame-aspectWindow) > 0.01 {
		if aspectWindow > aspectGame {
			// Window wider: normalize X
			scaleX := aspectWindow / aspectGame
			modelview = modelview.Mul4(mgl.Scale3D(scaleX, rp.vs, 1)) // pre-scale
			modelview = modelview.Mul4(rotMatrix())                   // rotate
			modelview = modelview.Mul4(mgl.Scale3D(1/scaleX, 1, 1))   // restore
		} else {
			// Window taller: normalize Y
			scaleY := aspectGame / aspectWindow
			modelview = modelview.Mul4(mgl.Scale3D(1, scaleY*rp.vs, 1))
			modelview = modelview.Mul4(rotMatrix())
			modelview = modelview.Mul4(mgl.Scale3D(1, 1/scaleY, 1))
		}
	} else {
		// Same aspect: simple rotation
		modelview = modelview.Mul4(mgl.Scale3D(1, rp.vs, 1))
		modelview = modelview.Mul4(rotMatrix())
	}

	return modelview
}

// ------------------------------------------------------------------
// Sprite rendering helpers (draw routines, tiling etc.)

func drawQuads(modelview mgl.Mat4, x1, y1, x2, y2, x3, y3, x4, y4 float32) {
	gfx.SetUniformMatrix("modelview", modelview[:])
	gfx.SetUniformF("x1x2x4x3", x1, x2, x4, x3) // this uniform is optional
	gfx.SetVertexData(
		x2, y2, 1, 1,
		x3, y3, 1, 0,
		x1, y1, 0, 1,
		x4, y4, 0, 0,
	)

	gfx.RenderQuad()
}

// Render a quad with optional horizontal tiling
func renderSpriteHTile(modelview mgl.Mat4, x1, y1, x2, y2, x3, y3, x4, y4, dy, width float32, rp RenderParams) {
	//            p3
	//    p4 o-----o-----o- - -o
	//      /      |      \     ` .
	//     /       |       \       `.
	//    o--------o--------o- - - - o
	//   p1         p2
	topdist := (x3 - x4) * (((float32(rp.tile.xspacing) + width) / rp.xas) / width)
	botdist := (x2 - x1) * (((float32(rp.tile.xspacing) + width) / rp.xas) / width)
	if AbsF(topdist) >= 0.01 {
		db := (x4 - rp.rcx) * (botdist - topdist) / AbsF(topdist)
		x1 += db
		x2 += db
	}

	// Compute left/right tiling bounds (or right/left when topdist < 0)
	xmax := float32(sys_scrrect[2])
	left, right := int32(0), int32(1)
	if rp.tile.xflag != 0 {
		if rp.projectionMode == 0 {
			// Original culling logic (only when no projection)
			if topdist >= 0.01 {
				if x1 > x2 {
					left = 1 - int32(math.Ceil(float64(MaxF(x4/topdist, x1/botdist))))
					right = int32(math.Ceil(float64(MaxF((xmax-x3)/topdist, (xmax-x2)/botdist))))
				} else {
					left = 1 - int32(math.Ceil(float64(MaxF(x3/topdist, x2/botdist))))
					right = int32(math.Ceil(float64(MaxF((xmax-x4)/topdist, (xmax-x1)/botdist))))
				}
			} else if topdist <= -0.01 {
				if x1 > x2 {
					left = 1 - int32(math.Ceil(float64(MaxF((xmax-x3)/-topdist, (xmax-x2)/-botdist))))
					right = int32(math.Ceil(float64(MaxF(x4/-topdist, x1/-botdist))))
				} else {
					left = 1 - int32(math.Ceil(float64(MaxF((xmax-x4)/-topdist, (xmax-x1)/-botdist))))
					right = int32(math.Ceil(float64(MaxF(x3/-topdist, x2/-botdist))))
				}
			}
			if rp.tile.xflag != 1 {
				left = 0
				right = Min(right, Max(rp.tile.xflag, 1))
			}
		} else {
			// When projection is active: skip horizontal culling (geometry distortion breaks it)
			// Instead, use a fixed symmetric range based on xflag to avoid infinite tiling
			left = 1 - rp.tile.xflag
			right = rp.tile.xflag
		}
	}

	// Draw all quads in one loop
	for n := left; n < right; n++ {
		x1d, x2d := x1+float32(n)*botdist, x2+float32(n)*botdist
		x3d, x4d := x3+float32(n)*topdist, x4+float32(n)*topdist
		mat := modelview
		if !rp.rot.IsZero() {
			mat = applyProjection(mat, rp, int(n), botdist, dy)
			mat = applyRotation(mat, rp)
			mat = mat.Mul4(mgl.Translate3D(-(rp.rcx + float32(n)*botdist), -(rp.rcy + dy), 0))
		}

		drawQuads(mat, x1d, y1, x2d, y2, x3d, y3, x4d, y4)
	}
}
func renderSpriteQuad(modelview mgl.Mat4, rp RenderParams) {
	x1, y1 := rp.x, rp.rcy+((rp.y-rp.ys*float32(rp.size[1]))-rp.rcy)*rp.vs
	x2, y2 := x1+rp.xbs*float32(rp.size[0]), y1
	x3, y3 := rp.x+rp.xts*float32(rp.size[0]), rp.rcy+(rp.y-rp.rcy)*rp.vs
	x4, y4 := rp.x, y3
	//var pers float32
	//if AbsF(rp.xts) < AbsF(rp.xbs) {
	//	pers = AbsF(rp.xts) / AbsF(rp.xbs)
	//} else {
	//	pers = AbsF(rp.xbs) / AbsF(rp.xts)
	//}
	if !rp.rot.IsZero() && rp.tile.xflag == 0 && rp.tile.yflag == 0 {

		if rp.vs != 1 {
			y1 = rp.rcy + ((rp.y - rp.ys*float32(rp.size[1])) - rp.rcy)
			y2 = y1
			y3 = rp.y
			y4 = y3
		}
		modelview = applyProjection(modelview, rp, 0, 1, 0)
		// Apply shear matrix before rotation
		shearMatrix := mgl.Mat4{
			1, 0, 0, 0,
			rp.rxadd, 1, 0, 0,
			0, 0, 1, 0,
			0, 0, 0, 1}
		modelview = modelview.Mul4(shearMatrix)
		modelview = modelview.Mul4(mgl.Translate3D(rp.rxadd*rp.ys*float32(rp.size[1]), 0, 0))

		modelview = applyRotation(modelview, rp)
		modelview = modelview.Mul4(mgl.Translate3D(-rp.rcx, -rp.rcy, 0))

		drawQuads(modelview, x1, y1, x2, y2, x3, y3, x4, y4)
		return
	}
	if rp.tile.yflag == 1 && rp.xbs != 0 {
		x1 += rp.rxadd * rp.ys * float32(rp.size[1])
		x2 = x1 + rp.xbs*float32(rp.size[0])
		x1d, y1d, x2d, y2d, x3d, y3d, x4d, y4d := x1, y1, x2, y2, x3, y3, x4, y4
		n := 0
		var xy []float32
		for {
			x1d, y1d = x4d, y4d+rp.ys*rp.vs*((float32(rp.tile.yspacing)+float32(rp.size[1]))/rp.yas-float32(rp.size[1]))
			x2d, y2d = x3d, y1d
			x3d = x4d - rp.rxadd*rp.ys*float32(rp.size[1]) + (rp.xts/rp.xbs)*(x3d-x4d)
			y3d = y2d + rp.ys*rp.vs*float32(rp.size[1])
			x4d = x4d - rp.rxadd*rp.ys*float32(rp.size[1])
			if AbsF(y3d-y4d) < 0.01 {
				break
			}
			y4d = y3d
			if rp.ys*((float32(rp.tile.yspacing)+float32(rp.size[1]))/rp.yas) < 0 {
				if y1d <= float32(-sys_scrrect[3]) && y4d <= float32(-sys_scrrect[3]) {
					break
				}
			} else if y1d >= 0 && y4d >= 0 {
				break
			}
			n += 1
			xy = append(xy, x1d, x2d, x3d, x4d, y1d, y2d, y3d, y4d)
		}
		for {
			if len(xy) == 0 {
				break
			}
			x1d, x2d, x3d, x4d, y1d, y2d, y3d, y4d, xy = xy[len(xy)-8], xy[len(xy)-7], xy[len(xy)-6], xy[len(xy)-5], xy[len(xy)-4], xy[len(xy)-3], xy[len(xy)-2], xy[len(xy)-1], xy[:len(xy)-8]
			if (0 > y1d || 0 > y4d) &&
				(y1d > float32(-sys_scrrect[3]) || y4d > float32(-sys_scrrect[3])) {
				renderSpriteHTile(modelview, x1d, y1d, x2d, y2d, x3d, y3d, x4d, y4d, y1d-y1, float32(rp.size[0]), rp)
			}
		}
	}
	if rp.tile.yflag == 0 || rp.xts != 0 {
		x1 += rp.rxadd * rp.ys * float32(rp.size[1])
		x2 = x1 + rp.xbs*float32(rp.size[0])
		n := rp.tile.yflag
		oy := y1
		for {
			if rp.ys*((float32(rp.tile.yspacing)+float32(rp.size[1]))/rp.yas) > 0 {
				if y1 <= float32(-sys_scrrect[3]) && y4 <= float32(-sys_scrrect[3]) {
					break
				}
			} else if y1 >= 0 && y4 >= 0 {
				break
			}
			if (0 > y1 || 0 > y4) &&
				(y1 > float32(-sys_scrrect[3]) || y4 > float32(-sys_scrrect[3])) {
				renderSpriteHTile(modelview, x1, y1, x2, y2, x3, y3, x4, y4, y1-oy, float32(rp.size[0]), rp)
			}
			if rp.tile.yflag != 1 && n != 0 {
				n--
			}
			if n == 0 {
				break
			}
			x4, y4 = x1, y1-rp.ys*rp.vs*((float32(rp.tile.yspacing)+float32(rp.size[1]))/rp.yas-float32(rp.size[1]))
			x3, y3 = x2, y4
			x2 = x1 + rp.rxadd*rp.ys*float32(rp.size[1]) + (rp.xbs/rp.xts)*(x2-x1)
			y2 = y3 - rp.ys*rp.vs*float32(rp.size[1])
			x1 = x1 + rp.rxadd*rp.ys*float32(rp.size[1])
			if AbsF(y1-y2) < 0.01 {
				break
			}
			y1 = y2
		}
	}
}
func renderWithBlending(
	render func(eq BlendEquation, src, dst BlendFunc, a float32),
	blendMode TransType, blendAlpha [2]int32, correctAlpha bool, invblend int32, neg *bool, acolor *[3]float32, mcolor *[3]float32, isrgba bool) {

	blendSourceFactor := BlendSrcAlpha
	if !correctAlpha {
		blendSourceFactor = BlendOne
	}

	Blend := BlendAdd
	BlendI := BlendReverseSubtract
	if invblend >= 1 {
		Blend = BlendReverseSubtract
		BlendI = BlendAdd
	}

	src := blendAlpha[0]
	dst := blendAlpha[1]

	// Ensure proper source and destination
	// TODO: Maybe use byte everywhere
	src = Clamp(src, 0, 255)
	dst = Clamp(dst, 0, 255)

	// Force None destination to 0 just in case
	if blendMode == TT_none {
		dst = 0
	}

	switch {
	// Sub
	case blendMode == TT_sub:
		if src == 0 && dst == 255 {
			// Fully transparent. Skip render
		} else if src == 255 && dst == 255 {
			// Fast path for full subtraction
			if invblend >= 1 && acolor != nil {
				(*acolor)[0], (*acolor)[1], (*acolor)[2] = -acolor[0], -acolor[1], -acolor[2]
			}
			if invblend == 3 && neg != nil {
				*neg = false
			}
			render(BlendI, blendSourceFactor, BlendOne, 1)
		} else {
			// Full alpha range
			if dst < 255 {
				render(BlendAdd, BlendZero, BlendOneMinusSrcAlpha, 1-float32(dst)/255)
			}
			if src > 0 {
				if invblend >= 1 && acolor != nil {
					(*acolor)[0], (*acolor)[1], (*acolor)[2] = -acolor[0], -acolor[1], -acolor[2]
				}
				if invblend == 3 && neg != nil {
					*neg = false
				}
				render(BlendI, blendSourceFactor, BlendOne, float32(src)/255)
			}
		}
	// Add, None or Default
	// None takes this path because SuperPause darkens sprites through their source alpha
	// Default should normally not reach here, so this is only a fallback
	default:
		if src == 0 && dst == 255 {
			// Fully transparent. Just don't render
		} else if src == 255 && dst == 0 {
			// Fast path for fully opaque
			render(BlendAdd, blendSourceFactor, BlendOneMinusSrcAlpha, 1)
		} else if src == 255 && dst == 255 {
			// Fast path for full Add
			if invblend >= 1 && acolor != nil {
				(*acolor)[0], (*acolor)[1], (*acolor)[2] = -acolor[0], -acolor[1], -acolor[2]
			}
			if invblend == 3 && neg != nil {
				*neg = false
			}
			render(Blend, blendSourceFactor, BlendOne, 1)
		} else {
			// AddAlpha (includes Add1)
			if dst < 255 {
				render(Blend, BlendZero, BlendOneMinusSrcAlpha, 1-float32(dst)/255)
			}
			if src > 0 {
				if invblend >= 1 && dst >= 255 {
					if invblend >= 2 {
						if invblend == 3 && neg != nil {
							*neg = false
						}
						if acolor != nil {
							(*acolor)[0], (*acolor)[1], (*acolor)[2] = -acolor[0], -acolor[1], -acolor[2]
						}
					}
					Blend = BlendReverseSubtract
				} else {
					Blend = BlendAdd
				}
				if !isrgba && (invblend >= 2 || invblend <= -1) && acolor != nil && mcolor != nil && src < 255 {
					// Sum of add components
					gc := AbsF(acolor[0]) + AbsF(acolor[1]) + AbsF(acolor[2])
					v3, ml, al := MaxF((gc*255)-float32(dst+src), 512)/128, (float32(src) / 255), (float32(src+dst) / 255)
					rM, gM, bM := mcolor[0]*ml, mcolor[1]*ml, mcolor[2]*ml
					(*mcolor)[0], (*mcolor)[1], (*mcolor)[2] = rM, gM, bM
					render(Blend, blendSourceFactor, BlendOne, al*Pow(v3, 3))
				} else {
					render(Blend, blendSourceFactor, BlendOne, float32(src)/255)
				}
			}
		}

	}
}
func RenderSprite(rp RenderParams) {
	if !rp.IsValid() {
		fmt.Println("RenderSprite: Invalid parameters, skipping")
		return
	}

	initRenderSpriteQuad(&rp)
	neg, grayscale, padd, pmul, invblend, hue := false, float32(0), [3]float32{0, 0, 0}, [3]float32{1, 1, 1}, int32(0), float32(0)
	tint := [4]float32{float32(rp.tint&0xff) / 255, float32(rp.tint>>8&0xff) / 255,
		float32(rp.tint>>16&0xff) / 255, float32(rp.tint>>24&0xff) / 255}

	if rp.pfx != nil {
		neg, grayscale, padd, pmul, invblend, hue = rp.pfx.getFcPalFx(false, rp.blendAlpha)
	}

	proj := gfx.OrthographicProjectionMatrix(0, float32(sys_scrrect[2]), 0, float32(sys_scrrect[3]), -65535, 65535)
	modelview := mgl.Translate3D(0, float32(sys_scrrect[3]), 0)

	gfx.Scissor(rp.window[0], rp.window[1], rp.window[2], rp.window[3])

	render := func(eq BlendEquation, src, dst BlendFunc, a float32) {
		gfx.SetPipeline(eq, src, dst)

		gfx.SetUniformMatrix("projection", proj[:])
		gfx.SetTexture("tex", rp.tex)
		if rp.paltex == nil {
			gfx.SetUniformI("isRgba", 1)
		} else {
			gfx.SetTexture("pal", rp.paltex)
			gfx.SetUniformI("isRgba", 0)
		}
		gfx.SetUniformI("mask", int(rp.mask))
		gfx.SetUniformI("isTrapez", int(Btoi(AbsF(AbsF(rp.xts)-AbsF(rp.xbs)) > 0.001)))
		gfx.SetUniformI("isFlat", 0)

		gfx.SetUniformI("neg", int(Btoi(neg)))
		gfx.SetUniformF("gray", grayscale)
		gfx.SetUniformF("hue", hue)
		gfx.SetUniformFv("add", padd[:])
		gfx.SetUniformFv("mult", pmul[:])
		gfx.SetUniformFv("tint", tint[:])
		gfx.SetUniformF("alpha", a)

		// If an atlas UV rectangle is provided use it in the shader
		useUV := 0
		if (rp.uv[2]-rp.uv[0]) > 1e-6 && (rp.uv[3]-rp.uv[1]) > 1e-6 {
			useUV = 1
		}
		gfx.SetUniformFv("uvRect", rp.uv[:])
		gfx.SetUniformI("useUV", useUV)

		renderSpriteQuad(modelview, rp)

		gfx.ReleasePipeline()
	}

	renderWithBlending(render, rp.blendMode, rp.blendAlpha, rp.paltex != nil, invblend, &neg, &padd, &pmul, rp.paltex == nil)
	gfx.DisableScissor()
}

// ------------------------------------------------------------------
// Sprite and palette helpers / Sprite methods

func PaletteToTexture(pal []uint32) Texture {
	tx := gfx.newPaletteTexture()

	// Safely handle invalid palettes
	if len(pal) == 0 {
		fmt.Printf("Invalid palette texture. Defaulting to none\n")
		tx.SetData(nil)
		return tx
	} else {
		tx.SetData(unsafe.Slice((*byte)(unsafe.Pointer(&pal[0])), len(pal)*4))
		return tx
	}
}

// Compare current palette to previous one and reuse if possible
// This saves a lot of palette operations when the same player has many sprites on screen
func (s *Sprite) CachePalette(pal []uint32) Texture {
	hasPalette := true
	if s.PalTex == nil || len(pal) != len(s.paltemp) {
		hasPalette = false
	} else {
		for i := range pal {
			if pal[i] != s.paltemp[i] {
				hasPalette = false
				break
			}
		}
	}
	// If cached texture is invalid, generate a new one and cache it
	if !hasPalette {
		s.PalTex = PaletteToTexture(pal)
		s.paltemp = append([]uint32{}, pal...)
	}
	return s.PalTex
}
func (s *Sprite) Draw(x, y, xscale, yscale float32, rxadd float32, rot Rotation, fx *PalFX, window *[4]int32) {
	x += float32(sys_gameWidth-320)/2 - xscale*float32(s.Offset[0])
	y += float32(sys_gameHeight-240) - yscale*float32(s.Offset[1])
	var rcx, rcy float32

	if rot.IsZero() {
		if xscale < 0 {
			x *= -1
		}
		if yscale < 0 {
			y *= -1
		}
		rcx, rcy = rcx*sys_widthScale, 0
	} else {
		rcx, rcy = (x+rcx)*sys_widthScale, y*sys_heightScale
		x, y = AbsF(xscale)*float32(s.Offset[0]), AbsF(yscale)*float32(s.Offset[1])
	}

	rp := RenderParams{
		tex:            s.Tex,
		paltex:         s.PalTex,
		size:           s.Size,
		x:              -x * sys_widthScale,
		y:              -y * sys_heightScale,
		tile:           notiling,
		xts:            xscale * sys_widthScale,
		xbs:            xscale * sys_widthScale,
		ys:             yscale * sys_heightScale,
		vs:             1,
		rxadd:          rxadd,
		xas:            1,
		yas:            1,
		rot:            rot,
		tint:           0,
		blendMode:      TT_add,
		blendAlpha:     [2]int32{int32(255 * sys_brightness), 0},
		mask:           0,
		pfx:            fx,
		window:         window,
		rcx:            rcx,
		rcy:            rcy,
		projectionMode: 0,
		fLength:        0,
		xOffset:        -xscale * float32(s.Offset[0]),
		yOffset:        -yscale * float32(s.Offset[1]),
		uv:             s.UV,
	}
	RenderSprite(rp)
}

// (font types declared in this file's types section)

// CharWidth returns the width that has a specified character
func (f *Fnt) CharWidth(c rune, bt int32) int32 {
	if c == ' ' {
		return int32(f.Size[0])
	}
	fci := f.images[bt][c]
	if fci == nil {
		return 0
	}
	return int32(fci.w)
}

// TextWidth returns the width that has a specified text.
// This depends on each char's width and font spacing
func (f *Fnt) TextWidth(txt string, bank int32) (w int32) {
	if f.BankType != "sprite" {
		bank = 0
	}
	for i, c := range txt {
		cw := f.CharWidth(c, bank)
		// in mugen negative spacing matching char width seems to skip calc,
		// even for 1 symbol string (which normally shouldn't use spacing)
		if cw+f.Spacing[0] > 0 {
			w += cw
			if i < len(txt)-1 {
				w += f.Spacing[0]
			}
		}
	}
	return
}
func (f *Fnt) getCharSpr(c rune, bank, bt int32) *Sprite {
	fci := f.images[bt][c]
	if fci == nil {
		return nil
	}

	if bank < int32(len(fci.img)) {
		return &fci.img[bank]
	}

	return &fci.img[0]
}

// True batch rendering implementation
func (f *Fnt) drawChars(glyphs []glyphDrawInfo, rot Rotation, window *[4]int32, palfx *PalFX, rp RenderParams) {
	if len(glyphs) == 0 {
		return
	}

	// Group glyphs by texture (font atlas)
	textureGroups := make(map[Texture][]glyphDrawInfo)
	for _, g := range glyphs {
		spr := f.getCharSpr(g.c, g.bank, g.bt)
		if spr == nil || spr.Tex == nil {
			continue
		}
		textureGroups[spr.Tex] = append(textureGroups[spr.Tex], g)
	}

	// For each texture group, render all characters using the standard RenderSprite
	for tex, texGlyphs := range textureGroups {
		for _, g := range texGlyphs {
			spr := f.getCharSpr(g.c, g.bank, g.bt)
			if spr == nil {
				continue
			}

			// Create render params for this character
			charRP := RenderParams{
				tex:            tex,
				paltex:         f.paltex,
				size:           spr.Size,
				x:              -g.x * sys_widthScale,
				y:              -g.y * sys_heightScale,
				tile:           notiling,
				xts:            g.xscl * sys_widthScale,
				xbs:            g.xscl * sys_widthScale,
				ys:             g.yscl * sys_heightScale,
				vs:             1,
				rxadd:          0,
				xas:            1,
				yas:            1,
				rot:            rot,
				tint:           0,
				blendMode:      rp.blendMode,
				blendAlpha:     rp.blendAlpha,
				mask:           0,
				pfx:            palfx,
				window:         window,
				rcx:            0,
				rcy:            0,
				projectionMode: 0,
				fLength:        0,
				xOffset:        0,
				yOffset:        0,
				uv:             spr.UV,
			}

			// Use the standard sprite rendering
			RenderSprite(charRP)
		}
	}
}

// Simple debug version - draw one character at a time
func (f *Fnt) drawCharsSimple(glyphs []glyphDrawInfo, rot Rotation, window *[4]int32, palfx *PalFX, rp RenderParams) {
	for _, g := range glyphs {
		spr := f.getCharSpr(g.c, g.bank, g.bt)
		if spr == nil || spr.Tex == nil {
			continue
		}

		// Create a simple render params for this character
		charRP := RenderParams{
			tex:            spr.Tex,
			paltex:         f.paltex,
			size:           spr.Size,
			x:              -g.x * sys_widthScale,
			y:              -g.y * sys_heightScale,
			tile:           notiling,
			xts:            g.xscl * sys_widthScale,
			xbs:            g.xscl * sys_widthScale,
			ys:             g.yscl * sys_heightScale,
			vs:             1,
			rxadd:          0,
			xas:            1,
			yas:            1,
			rot:            rot,
			tint:           0,
			blendMode:      rp.blendMode,
			blendAlpha:     rp.blendAlpha,
			mask:           0,
			pfx:            palfx,
			window:         window,
			rcx:            0,
			rcy:            0,
			projectionMode: 0,
			fLength:        0,
			xOffset:        0,
			yOffset:        0,
			uv:             spr.UV,
		}

		// Use the standard sprite rendering
		RenderSprite(charRP)
	}
}

func (f *Fnt) drawCharsBatch(
	glyphs []glyphDrawInfo,
	rot Rotation,
	window *[4]int32,
	palfx *PalFX,
	rp RenderParams,
) {
	if len(glyphs) == 0 {
		fmt.Printf("drawCharsBatch: No glyphs to render\n")
		return
	}

	// ---- Configure pipeline once ----
	gfx.SetPipeline(BlendAdd, BlendSrcAlpha, BlendOneMinusSrcAlpha)

	proj := gfx.OrthographicProjectionMatrix(0, float32(sys_scrrect[2]), 0, float32(sys_scrrect[3]), -65535, 65535)
	modelview := mgl.Ident4() // Identity - no translation needed since we'll use correct Y in vertices

	gfx.SetUniformMatrix("projection", proj[:])
	gfx.SetUniformMatrix("modelview", modelview[:])
	gfx.SetUniformI("mask", 0)
	gfx.SetUniformI("isFlat", 0)
	gfx.SetUniformI("isTrapez", 0)

	// PalFX
	if f.paltex != nil {
		gfx.SetTexture("pal", f.paltex)
		gfx.SetUniformI("isRgba", 0)
	} else {
		gfx.SetUniformI("isRgba", 1)
	}

	neg, gray, padd, pmul, _, hue :=
		func() (bool, float32, [3]float32, [3]float32, int32, float32) {
			if palfx == nil {
				return false, 0, [3]float32{}, [3]float32{1, 1, 1}, 0, 0
			}
			return palfx.getFcPalFx(false, rp.blendAlpha)
		}()

	gfx.SetUniformI("neg", int(Btoi(neg)))
	gfx.SetUniformF("gray", gray)
	gfx.SetUniformF("hue", hue)
	gfx.SetUniformFv("add", padd[:])
	gfx.SetUniformFv("mult", pmul[:])
	gfx.SetUniformFv("tint", []float32{1, 1, 1, 1})
	gfx.SetUniformF("alpha", float32(rp.blendAlpha[0])/255.0)

	gfx.Scissor(window[0], window[1], window[2], window[3])
	defer gfx.DisableScissor()

	// ---- Group glyphs by texture ----
	textureGroups := make(map[Texture][]glyphDrawInfo)
	validGlyphCount := 0
	for _, g := range glyphs {
		spr := f.getCharSpr(g.c, g.bank, g.bt)
		if spr == nil || spr.Tex == nil || !spr.Tex.IsValid() {
			continue
		}
		textureGroups[spr.Tex] = append(textureGroups[spr.Tex], g)
		validGlyphCount++
	}

	if validGlyphCount == 0 {
		gfx.ReleasePipeline()
		return
	}

	// ---- For each texture group, build triangles and draw ----
	for tex, list := range textureGroups {
		gfx.SetTexture("tex", tex)
		gfx.SetUniformFv("uvRect", []float32{0, 0, 1, 1})
		gfx.SetUniformI("useUV", 0)

		// Build vertex data for all glyphs in this texture group
		triVerts := make([]float32, 0, len(list)*6*4)

		for _, g := range list {
			spr := f.getCharSpr(g.c, g.bank, g.bt)
			if spr == nil {
				continue
			}

			// Screen coordinates (top-left origin)
			screenX := g.x * sys_widthScale
			screenY := g.y * sys_heightScale
			w := float32(spr.Size[0]) * g.xscl * sys_widthScale
			h := float32(spr.Size[1]) * g.yscl * sys_heightScale

			// Convert to OpenGL coordinates (bottom-left origin)
			// screenY is from top, convert to from bottom
			glX := screenX
			glY := float32(sys_scrrect[3]) - screenY - h

			fmt.Printf("drawCharsBatch: Glyph '%c' screen(%.1f, %.1f) -> gl(%.1f, %.1f) size(%.1f, %.1f)\n",
				g.c, screenX, screenY, glX, glY, w, h)

			u1, v1, u2, v2 := spr.UV[0], spr.UV[1], spr.UV[2], spr.UV[3]
			if u2 == 0 && v2 == 0 {
				u1, v1, u2, v2 = 0, 0, 1, 1
			}

			// Quad in OpenGL space: bottom-left origin
			// Triangle 1: BL, BR, TL
			triVerts = append(triVerts,
				glX, glY, u1, v2, // bottom-left
				glX+w, glY, u2, v2, // bottom-right
				glX, glY+h, u1, v1, // top-left
			)
			// Triangle 2: BR, TR, TL
			triVerts = append(triVerts,
				glX+w, glY, u2, v2, // bottom-right
				glX+w, glY+h, u2, v1, // top-right
				glX, glY+h, u1, v1, // top-left
			)
		}

		if len(triVerts) == 0 {
			continue
		}

		// DEBUG output
		if len(triVerts) >= 12 {
			fmt.Printf("drawCharsBatch: First quad vertices (GL space):\n")
			fmt.Printf("  BL: (%.1f, %.1f) UV: (%. 3f, %.3f)\n", triVerts[0], triVerts[1], triVerts[2], triVerts[3])
			fmt.Printf("  BR: (%.1f, %.1f) UV: (%.3f, %.3f)\n", triVerts[4], triVerts[5], triVerts[6], triVerts[7])
			fmt.Printf("  TL: (%.1f, %.1f) UV: (%.3f, %.3f)\n", triVerts[8], triVerts[9], triVerts[10], triVerts[11])
		}

		// Upload and draw
		gfx.SetVertexData2(triVerts)
		vertexCount := int32(len(triVerts) / 4)

		gl.DrawArrays(gl.TRIANGLES, 0, vertexCount)
		sys_nDrawcall++

		if err := gl.GetError(); err != gl.NO_ERROR {
			fmt.Printf("drawCharsBatch: OpenGL error after draw: 0x%x\n", err)
		}
	}

	gfx.ReleasePipeline()
}

// MODIFIED: Fixed DrawText function with proper batch rendering
func (f *Fnt) DrawText(txt string, x, y, xscl, yscl, rxadd float32, rot Rotation, bank, align int32, window *[4]int32, palfx *PalFX, alpha float32) {
	if len(txt) == 0 || xscl == 0 || yscl == 0 {
		return
	}

	var bt int32
	if f.BankType == "sprite" {
		bt = bank
		bank = 0
	} else if bank < 0 || len(f.palettes) <= int(bank) {
		bank = 0
	}

	// Replace non-existing characters with space
	for i, c := range txt {
		if c != ' ' && f.images[bt][c] == nil {
			txt = txt[:i] + string(' ') + txt[i+1:]
		}
	}

	x += float32(f.offset[0])*xscl + float32(sys_gameWidth-320)/2
	y += float32(f.offset[1]-int32(f.Size[1])+1)*yscl + float32(sys_gameHeight-240)

	var rcx, rcy float32
	if rot.IsZero() {
		if xscl < 0 {
			x *= -1
		}
		if yscl < 0 {
			y *= -1
		}
		rcx, rcy = rcx*sys_widthScale, 0
	} else {
		rcx, rcy = (x+rcx)*sys_widthScale, y*sys_heightScale
		x, y = AbsF(xscl)*float32(f.offset[0]), AbsF(yscl)*float32(f.offset[1])
	}

	if align == 0 {
		x -= float32(f.TextWidth(txt, bank)) * xscl * 0.5
	} else if align < 0 {
		x -= float32(f.TextWidth(txt, bank)) * xscl
	}

	var pal []uint32
	if len(f.palettes) != 0 {
		pal = f.palettes[bank][:]
	}

	// Set up palette texture once for all characters
	f.paltex = nil
	if len(pal) > 0 {
		// Create a temporary sprite to generate the palette texture
		tempSpr := &Sprite{Pal: pal}
		f.paltex = tempSpr.CachePalette(pal)
	}

	// Set the trans type
	tt := TT_none
	if alpha < 1.0 {
		tt = TT_add
	}

	alphaVal := int32(255 * sys_brightness * alpha)

	// Initialize common render parameters for batch rendering
	rp := RenderParams{
		tex:            nil, // Will be set per character
		paltex:         f.paltex,
		size:           [2]uint16{0, 0}, // Will be set per character
		x:              0,               // Will be set per character
		y:              0,               // Will be set per character
		tile:           notiling,
		xts:            xscl * sys_widthScale,
		xbs:            xscl * sys_widthScale,
		ys:             yscl * sys_heightScale,
		vs:             1,
		rxadd:          rxadd,
		xas:            1,
		yas:            1,
		rot:            rot,
		tint:           0,
		blendMode:      tt,
		blendAlpha:     [2]int32{alphaVal, 255 - alphaVal},
		mask:           0,
		pfx:            palfx,
		window:         window,
		rcx:            rcx,
		rcy:            rcy,
		projectionMode: 0,
		fLength:        0,
		xOffset:        0,
		yOffset:        0,
		uv:             [4]float32{0, 0, 0, 0}, // Will be set per character
	}

	// Group characters by texture
	textureMap := make(map[Texture][]glyphDrawInfo)
	cx := x

	for _, c := range txt {
		if c == ' ' {
			cw := f.CharWidth(c, bank)
			if cw+f.Spacing[0] > 0 {
				cx += float32(cw) * xscl
				cx += xscl * float32(f.Spacing[0])
			}
			continue
		}

		spr := f.getCharSpr(c, bank, bt)
		if spr == nil || spr.Tex == nil {
			continue
		}

		glyph := glyphDrawInfo{
			x:    cx,
			y:    y,
			xscl: xscl,
			yscl: yscl,
			c:    c,
			bank: bank,
			bt:   bt,
			pal:  pal,
		}

		textureMap[spr.Tex] = append(textureMap[spr.Tex], glyph)

		cw := f.CharWidth(c, bank)
		if cw+f.Spacing[0] > 0 {
			cx += float32(cw) * xscl
			cx += xscl * float32(f.Spacing[0])
		}
	}

	// Render each texture group
	for tex, glyphs := range textureMap {
		// Set up render params for this texture group
		groupRP := rp
		groupRP.tex = tex

		// In your DrawText function, before calling drawCharsBatch:
		fmt.Printf("DrawText: Calling drawCharsBatch with %d glyphs\n", len(glyphs))
		fmt.Printf("DrawText: Screen rect: %v\n", sys_scrrect)
		fmt.Printf("DrawText: Width scale: %.2f, Height scale: %.2f\n", sys_widthScale, sys_heightScale)

		// Use drawChars for this specific texture group
		// f.drawChars(glyphs, rot, window, palfx, groupRP) // WORKING
		// f.drawCharsSimple(glyphs, rot, window, palfx, groupRP) // WORKING
		f.drawCharsBatch(glyphs, rot, window, palfx, groupRP) // WORKING

		// Check state after call
		// checkGLState("After drawCharsBatch")
	}
}

func newFnt() *Fnt {
	return &Fnt{
		images:   make(map[int32]map[rune]*FntCharImage),
		BankType: "palette",
	}
}
func LoadText(filename string) (string, error) {
	rc, err := OpenFile(filename)
	if err != nil {
		return "", err
	}
	defer rc.Close()

	bytes, err := io.ReadAll(rc)
	if err != nil {
		return "", err
	}

	if len(bytes) >= 3 && bytes[0] == 0xef && bytes[1] == 0xbb && bytes[2] == 0xbf { // UTF-8 BOM
		return string(bytes[3:]), nil
	}

	return string(bytes), nil
}

// Split string on separator, and remove all
// leading and trailing white space from each line
func SplitAndTrim(str, sep string) (ss []string) {
	ss = strings.Split(str, sep)
	for i, s := range ss {
		ss[i] = strings.TrimSpace(s)
	}
	return
}
func SectionName(sec string) (string, string) {
	if len(sec) == 0 || sec[0] != '[' {
		return "", ""
	}
	sec = strings.TrimSpace(strings.SplitN(sec, ";", 2)[0])
	if sec[len(sec)-1] != ']' {
		return "", ""
	}
	sec = sec[1:strings.Index(sec, "]")]
	var name string
	i := strings.Index(sec, " ")
	if i >= 0 {
		name = sec[:i+1]
		sec = sec[i+1:]
	} else {
		name = sec
		sec = ""
	}
	return strings.ToLower(name), sec
}

type IniSection map[string]string

func NewIniSection() IniSection {
	return IniSection(make(map[string]string))
}
func (is IniSection) Parse(lines []string, i *int) {
	for ; *i < len(lines); (*i)++ {
		if len(lines[*i]) > 0 && lines[*i][0] == '[' {
			break
		}
		line := strings.TrimSpace(strings.SplitN(lines[*i], ";", 2)[0])
		ia := strings.IndexAny(line, "= \t")
		if ia > 0 {
			name := strings.ToLower(line[:ia])
			var data string
			ia = strings.Index(line, "=")
			if ia >= 0 {
				data = strings.TrimSpace(line[ia+1:])
			}
			_, ok := is[name]
			if !ok {
				is[name] = data
			}
		}
	}
}
func I32ToU16(i32 int32) uint16 {
	if i32 < 0 {
		return 0
	}
	if i32 > int32(^uint16(0)) {
		return ^uint16(0)
	}
	return uint16(i32)
}
func (s *Sff) GetSprite(g, n uint16) *Sprite {
	if g == 0xFFFF {
		return nil
	}
	return s.sprites[[2]uint16{g, n}]
}
func (s *Sff) getOwnPalSprite(g, n uint16, pl *PaletteList) *Sprite {
	sys_runMainThreadTask() // Generate texture
	sp := s.GetSprite(g, n)
	if sp == nil {
		return nil
	}
	osp, pal := *sp, sp.GetPal(pl)
	osp.Pal = make([]uint32, len(pal))
	copy(osp.Pal, pal)
	return &osp
}
func LoadFntSff(f *Fnt, fontfile string, filename string) {
	fmt.Printf("fontfile=%v filename=%v\n", fontfile, filename)
	fileDir := filename
	sff, err := loadSff(fileDir, false)

	if err != nil {
		panic(err)
	}

	// Load sprites
	var pal_default []uint32
	for k, sprite := range sff.sprites {
		s := sff.getOwnPalSprite(sprite.Group, sprite.Number, &sff.palList)
		if sprite.Group == 0 || f.BankType == "sprite" {
			if f.images[int32(sprite.Group)] == nil {
				f.images[int32(sprite.Group)] = make(map[rune]*FntCharImage)
			}
			if pal_default == nil && sff.header.Ver0 == 1 {
				pal_default = s.Pal
			}
			offsetX := uint16(s.Offset[0])
			sizeX := uint16(s.Size[0])

			fci := &FntCharImage{
				ofs: offsetX,
				w:   sizeX,
			}
			fci.img = make([]Sprite, 1)
			fci.img[0] = *s
			f.images[int32(sprite.Group)][rune(k[1])] = fci
		}
	}

	// Load palettes
	f.palettes = make([][256]uint32, sff.header.NumberOfPalettes)
	f.coldepth = make([]byte, sff.header.NumberOfPalettes)
	var idef int
	for i := 0; i < int(sff.header.NumberOfPalettes); i++ {
		var pal []uint32
		si, ok := sff.palList.PalTable[[...]uint16{0, uint16(i)}]
		if ok && si >= 0 {
			pal = sff.palList.Get(si)
			if i == 0 {
				idef = si
			}
			switch sff.palList.numcols[[...]uint16{0, uint16(i)}] {
			case 256:
				f.coldepth[i] = 8
			case 32:
				f.coldepth[i] = 5
			}
		} else {
			pal = sff.palList.Get(idef)
		}
		copy(f.palettes[i][:], pal)
	}
	if len(f.palettes) == 0 && pal_default != nil {
		f.palettes = make([][256]uint32, 1)
		copy(f.palettes[0][:], pal_default)
	}
}
func loadDefInfo(f *Fnt, filename string, is IniSection, height int32) {
	f.Type = strings.ToLower(is["type"])
	if _, ok := is["banktype"]; ok {
		f.BankType = strings.ToLower(is["banktype"])
	}
	ary := SplitAndTrim(is["size"], ",")
	if len(ary[0]) > 0 {
		f.Size[0] = I32ToU16(Atoi(ary[0]))
	}
	if len(ary) > 1 && len(ary[1]) > 0 {
		f.Size[1] = I32ToU16(Atoi(ary[1]))
	}
	ary = SplitAndTrim(is["spacing"], ",")
	if len(ary[0]) > 0 {
		f.Spacing[0] = Atoi(ary[0])
	}
	if len(ary) > 1 && len(ary[1]) > 0 {
		f.Spacing[1] = Atoi(ary[1])
	}
	f.colors = Clamp(Atoi(is["colors"]), 1, 255)
	ary = SplitAndTrim(is["offset"], ",")
	if len(ary[0]) > 0 {
		f.offset[0] = Atoi(ary[0])
	}
	if len(ary) > 1 && len(ary[1]) > 0 {
		f.offset[1] = Atoi(ary[1])
	}

	if len(is["file"]) > 0 {
		if f.Type == "truetype" {
			// LoadFntTtf(f, filename, is["file"], height)
		} else {
			LoadFntSff(f, filename, is["file"])
		}
	}
}
func Atoi(str string) int32 {
	var n int64
	str = strings.TrimSpace(str)
	if len(str) > 0 {
		var a string
		if str[0] == '-' || str[0] == '+' {
			a = str[1:]
		} else {
			a = str
		}
		for i := range a {
			if a[i] < '0' || '9' < a[i] {
				break
			}
			n = n*10 + int64(a[i]-'0')
			if n > 2147483647 {
				fmt.Printf("WARNING: Atoi conversion outside int32 range: %v", a[:i+1])
				fmt.Printf("Atoi conversion outside int32 range: %v\n", a[:i+1])
				if str[0] == '-' {
					return IErr
				}
				return IMax
			}
		}
		if str[0] == '-' {
			n *= -1
		}
	}
	return int32(n)
}
func loadFntV2(filename string, height int32) (*Fnt, error) {
	f := newFnt()

	content, err := LoadText(filename)

	if err != nil {
		return nil, Error("File not found")
	}

	lines := SplitAndTrim(string(content), "\n")
	i := 0
	var name string

	for ; i < len(lines); i++ {
		name, _ = SectionName(lines[i])
		if len(name) > 0 {
			is := NewIniSection()
			i++
			is.Parse(lines, &i)
			i--
			switch name {
			case "def":
				loadDefInfo(f, filename, is, height)
			}
		}
	}
	return f, nil
}

func (s *Sprite) GetPal(pl *PaletteList) []uint32 {
	if len(s.Pal) > 0 || s.coldepth > 8 {
		return s.Pal
	}
	return pl.Get(int(s.palidx)) //pl.palettes[pl.paletteMap[int(s.palidx)]]
}

// (TextureAtlas type moved to top types section)

func CreateTextureAtlas(width, height int32, depth int32, filter bool) *TextureAtlas {
	ta := &TextureAtlas{width: width, height: height, texture: gfx.newTexture(width, height, depth, filter), depth: depth, filter: filter, skyline: list.New(), resize: false}
	// Allocate backing storage for the atlas texture so subsequent TexSubImage2D
	// calls from AddImage will succeed. SetData(nil) calls gl.TexImage2D with
	// a nil pointer which creates an empty texture of the requested size.
	if ta.texture != nil {
		log.Printf("CreateTextureAtlas: allocating storage for atlas %dx%d depth=%d", width, height, depth)
		ta.texture.SetData(nil)
	}
	ta.skyline.PushBack([2]int32{0, 0})
	return ta
}
func (ta *TextureAtlas) AddImage(width, height int32, data []byte) ([4]float32, bool) {
	const maxWidth = 4096
	if ta.resize {
		if width > ta.width || height > ta.height {
			if width > maxWidth/2 || height > maxWidth/2 {
				return [4]float32{}, false
			}
			ta.Resize(width*2, height*2)
		}
	}
	x, y, ok := ta.FindPlaceToInsert(width, height)
	if !ok {
		if ta.resize {
			if ta.width != maxWidth && ta.height != maxWidth {
				ta.Resize(ta.width*2, ta.height*2)
			}
			x, y, ok = ta.FindPlaceToInsert(width, height)
		}
		if !ok {
			return [4]float32{}, false
		}
	}
	// Small diagnostic: sample first bytes of data for debugging uniform uploads
	if len(data) > 0 {
		sampleLen := 16
		if sampleLen > len(data) {
			sampleLen = len(data)
		}
		// only print if it looks suspicious (all equal) or short enough to be useful
		same := true
		for i := 1; i < sampleLen; i++ {
			if data[i] != data[0] {
				same = false
				break
			}
		}
		if same {
			log.Printf("AddImage: data sample uniform: %02x repeated (%d) first=%02x for image %dx%d at pos %d,%d", data[0], sampleLen, data[0], width, height, x, y)
		} else {
			log.Printf("AddImage: data sample first %d bytes: %v for image %dx%d at pos %d,%d", sampleLen, data[:sampleLen], width, height, x, y)
		}
	}
	ta.texture.SetSubData(data, x, y, width, height)
	return [4]float32{float32(x) / float32(ta.width), float32(y) / float32(ta.height), float32(x+width) / float32(ta.width), float32(y+height) / float32(ta.height)}, true
}
func (ta *TextureAtlas) FindPlaceToInsert(width, height int32) (int32, int32, bool) {
	//leave 1px space
	space := int32(1)
	width += space * 2
	height += space * 2
	var bestX int32 = math.MaxInt32
	var bestY int32 = math.MaxInt32
	var bestItr *list.Element = nil
	var bestItr2 *list.Element = nil
	for itr := ta.skyline.Front(); itr != nil; itr = itr.Next() {
		x := itr.Value.([2]int32)[0]
		y := itr.Value.([2]int32)[1]
		if width > ta.width-x {
			break
		}
		if y >= bestY {
			continue
		}
		xMax := x + width
		var itr2 *list.Element
		for itr2 = itr.Next(); itr2 != nil; itr2 = itr2.Next() {
			x2 := itr2.Value.([2]int32)[0]
			y2 := itr2.Value.([2]int32)[1]
			if xMax <= x2 {
				break
			}
			if y < y2 {
				y = y2
			}
		}
		if y >= bestY || height > ta.height-y {
			continue
		}
		bestItr = itr
		bestItr2 = itr2
		bestX = x
		bestY = y
	}
	if bestItr == nil {
		return 0, 0, false
	}
	ta.skyline.InsertBefore([2]int32{bestX, bestY + height}, bestItr)

	if bestItr2 == nil && bestX+width < ta.width {
		ta.skyline.InsertBefore([2]int32{bestX + width, ta.skyline.Back().Value.([2]int32)[1]}, bestItr)
	} else if bestItr2 != nil && bestX+width < bestItr2.Value.([2]int32)[0] {
		ta.skyline.InsertBefore([2]int32{bestX + width, bestItr2.Prev().Value.([2]int32)[1]}, bestItr)
	}
	itrNext := bestItr
	for itr := bestItr; itr != bestItr2; itr = itrNext {
		itrNext = itr.Next()
		ta.skyline.Remove(itr)
	}
	bestX += space
	bestY += space
	return bestX, bestY, true
}

func (ta *TextureAtlas) Resize(width, height int32) {
	if width < ta.width || height < ta.height {
		panic("New width cannot be smaller than old width")
	}
	if height < ta.height {
		panic("New height cannot be smaller than old height")
	}
	t := gfx.newTexture(width, height, ta.depth, ta.filter)
	t.CopyData(&ta.texture)
	ta.skyline.PushBack([2]int32{ta.width, 0})
	ta.width = width
	ta.height = height
	ta.texture = t
	return
}

// ------------------------------------------------------------------
// ShaderProgram_GL

// (ShaderProgram_GL type moved to top types section)

func (r *Renderer_GL) newShaderProgram(vert, frag, geo, id string, crashWhenFail bool) (s *ShaderProgram_GL, err error) {
	var vertObj, fragObj, geoObj, prog uint32
	if vertObj, err = r.compileShader(gl.VERTEX_SHADER, vert); chkEX(err, "Vertex Shader compilation error on "+id+"\n", crashWhenFail) {
		return nil, err
	}
	if fragObj, err = r.compileShader(gl.FRAGMENT_SHADER, frag); chkEX(err, "Fragment Shader compilation error on "+id+"\n", crashWhenFail) {
		return nil, err
	}
	if len(geo) > 0 {
		if geoObj, err = r.compileShader(gl.GEOMETRY_SHADER, geo); chkEX(err, "Geo Shader compilation error on "+id+"\n", crashWhenFail) {
			return nil, err
		}
		if prog, err = r.linkProgram(vertObj, fragObj, geoObj); chkEX(err, "Link program error on "+id+"\n", crashWhenFail) {
			return nil, err
		}
	} else {
		if prog, err = r.linkProgram(vertObj, fragObj); chkEX(err, "Link program error on "+id+"\n", crashWhenFail) {
			return nil, err
		}
	}
	s = &ShaderProgram_GL{program: prog}
	s.a = make(map[string]int32)
	s.u = make(map[string]int32)
	s.t = make(map[string]int)
	return s, nil
}
func (r *ShaderProgram_GL) glStr(s string) *uint8 {
	return gl.Str(s + "\x00")
}
func (s *ShaderProgram_GL) RegisterAttributes(names ...string) {
	for _, name := range names {
		s.a[name] = gl.GetAttribLocation(s.program, s.glStr(name))
	}
}

func (s *ShaderProgram_GL) RegisterUniforms(names ...string) {
	for _, name := range names {
		s.u[name] = gl.GetUniformLocation(s.program, s.glStr(name))
	}
}

func (s *ShaderProgram_GL) RegisterTextures(names ...string) {
	for _, name := range names {
		s.u[name] = gl.GetUniformLocation(s.program, s.glStr(name))
		s.t[name] = len(s.t)
	}
}

func (r *Renderer_GL) compileShader(shaderType uint32, src string) (shader uint32, err error) {
	shader = gl.CreateShader(shaderType)
	src = "#version 330\n" + src + "\x00"
	s, _ := gl.Strs(src)
	var l int32 = int32(len(src) - 1)
	gl.ShaderSource(shader, 1, s, &l)
	gl.CompileShader(shader)
	var ok int32
	gl.GetShaderiv(shader, gl.COMPILE_STATUS, &ok)
	if ok == 0 {
		//var err error
		var size, l int32
		gl.GetShaderiv(shader, gl.INFO_LOG_LENGTH, &size)
		if size > 0 {
			str := make([]byte, size+1)
			gl.GetShaderInfoLog(shader, size, &l, &str[0])
			err = Error(str[:l])
		} else {
			err = Error("Unknown shader compile error")
		}
		//chk(err)
		gl.DeleteShader(shader)
		//panic(Error("Shader compile error"))
		return 0, err
	}
	return shader, nil
}

func (r *Renderer_GL) linkProgram(params ...uint32) (program uint32, err error) {
	program = gl.CreateProgram()
	for _, param := range params {
		gl.AttachShader(program, param)
	}
	if len(params) > 2 {
		// Geometry Shader Params
		gl.ProgramParameteri(program, gl.GEOMETRY_INPUT_TYPE, gl.TRIANGLES)
		gl.ProgramParameteri(program, gl.GEOMETRY_OUTPUT_TYPE, gl.TRIANGLE_STRIP)
		gl.ProgramParameteri(program, gl.GEOMETRY_VERTICES_OUT, 3*6)
	}
	gl.LinkProgram(program)
	// Mark shaders for deletion when the program is deleted
	for _, param := range params {
		gl.DeleteShader(param)
	}
	var ok int32
	gl.GetProgramiv(program, gl.LINK_STATUS, &ok)
	if ok == 0 {
		//var err error
		var size, l int32
		gl.GetProgramiv(program, gl.INFO_LOG_LENGTH, &size)
		if size > 0 {
			str := make([]byte, size+1)
			gl.GetProgramInfoLog(program, size, &l, &str[0])
			err = Error(str[:l])
		} else {
			err = Error("Unknown link error")
		}
		//chk(err)
		gl.DeleteProgram(program)
		//panic(Error("Link error"))
		return 0, err
	}
	return program, nil
}

// ------------------------------------------------------------------
// Texture_GL

// (Texture_GL type moved to top types section)

// Generate a new texture name
func (r *Renderer_GL) newTexture(width, height, depth int32, filter bool) (t Texture) {
	var h uint32
	gl.ActiveTexture(gl.TEXTURE0)
	gl.GenTextures(1, &h)
	t = &Texture_GL{width, height, depth, filter, h}
	runtime.SetFinalizer(t, func(t *Texture_GL) {
		sys_mainThreadTask <- func() {
			gl.DeleteTextures(1, &t.handle)
		}
	})
	return
}

func (r *Renderer_GL) newPaletteTexture() Texture {
	return r.newTexture(256, 1, 32, false)
}

func (r *Renderer_GL) newModelTexture(width, height, depth int32, filter bool) Texture {
	return r.newTexture(width, height, depth, filter)
}

func (r *Renderer_GL) newDataTexture(width, height int32) (t Texture) {
	var h uint32
	gl.ActiveTexture(gl.TEXTURE0)
	gl.GenTextures(1, &h)
	t = &Texture_GL{width, height, 128, false, h}
	runtime.SetFinalizer(t, func(t *Texture_GL) {
		sys_mainThreadTask <- func() {
			gl.DeleteTextures(1, &t.handle)
		}
	})
	gl.BindTexture(gl.TEXTURE_2D, h)
	//gl.TexImage2D(gl.TEXTURE_2D, 0, 32, t.width, t.height, 0, 36, gl.FLOAT, nil)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
	return
}
func (r *Renderer_GL) newHDRTexture(width, height int32) (t Texture) {
	var h uint32
	gl.ActiveTexture(gl.TEXTURE0)
	gl.GenTextures(1, &h)
	t = &Texture_GL{width, height, 96, false, h}
	runtime.SetFinalizer(t, func(t *Texture_GL) {
		sys_mainThreadTask <- func() {
			gl.DeleteTextures(1, &t.handle)
		}
	})
	gl.BindTexture(gl.TEXTURE_2D, h)

	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.MIRRORED_REPEAT)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.MIRRORED_REPEAT)
	return
}
func (r *Renderer_GL) newCubeMapTexture(widthHeight int32, mipmap bool, lowestMipLevel int32) (t Texture) {
	var h uint32
	gl.ActiveTexture(gl.TEXTURE0)
	gl.GenTextures(1, &h)
	t = &Texture_GL{widthHeight, widthHeight, 24, false, h}
	runtime.SetFinalizer(t, func(t *Texture_GL) {
		sys_mainThreadTask <- func() {
			gl.DeleteTextures(1, &t.handle)
		}
	})
	gl.BindTexture(gl.TEXTURE_CUBE_MAP, h)
	for i := 0; i < 6; i++ {
		gl.TexImage2D(uint32(gl.TEXTURE_CUBE_MAP_POSITIVE_X+i), 0, gl.RGB32F, widthHeight, widthHeight, 0, gl.RGB, gl.FLOAT, nil)
	}
	if mipmap {
		gl.TexParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
		gl.GenerateMipmap(gl.TEXTURE_CUBE_MAP)
	} else {
		gl.TexParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	}

	gl.TexParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
	return
}

// Bind a texture and upload texel data to it
func (t *Texture_GL) SetData(data []byte) {
	var interp int32 = gl.NEAREST
	if t.filter {
		interp = gl.LINEAR
	}

	format := t.MapInternalFormat(Max(t.depth, 8))

	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	// Ensure any pending GL commands which upload texture data finish before
	// reading with GetTexImage. This avoids reading an incomplete texture on
	// some drivers where uploads are deferred.
	gl.Finish()
	gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1)
	if data != nil {
		gl.TexImage2D(gl.TEXTURE_2D, 0, int32(format), t.width, t.height, 0, format, gl.UNSIGNED_BYTE, unsafe.Pointer(&data[0]))
	} else {
		gl.TexImage2D(gl.TEXTURE_2D, 0, int32(format), t.width, t.height, 0, format, gl.UNSIGNED_BYTE, nil)
	}

	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, interp)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, interp)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
}
func (t *Texture_GL) SetSubData(data []byte, x, y, width, height int32) {
	// var interp int32 = gl.NEAREST
	// if t.filter {
	// 	interp = gl.LINEAR
	// }
	format := t.MapInternalFormat(Max(t.depth, 8))

	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1)
	if data != nil {
		gl.TexSubImage2D(
			gl.TEXTURE_2D,
			0,
			x, y,
			width, height,
			format,
			gl.UNSIGNED_BYTE,
			unsafe.Pointer(&data[0]),
		)
	} else {
		gl.TexSubImage2D(
			gl.TEXTURE_2D,
			0,
			x, y,
			width, height,
			format,
			gl.UNSIGNED_BYTE,
			nil,
		)
	}
	// gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, interp)
	// gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, interp)
	// gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	// gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
}
func (t *Texture_GL) SetDataG(data []byte, mag, min, ws, wt TextureSamplingParam) {

	format := t.MapInternalFormat(Max(t.depth, 8))

	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1)
	gl.TexImage2D(gl.TEXTURE_2D, 0, int32(format), t.width, t.height, 0, format, gl.UNSIGNED_BYTE, unsafe.Pointer(&data[0]))
	gl.GenerateMipmap(gl.TEXTURE_2D)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, t.MapTextureSamplingParam(mag))
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, t.MapTextureSamplingParam(min))
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, t.MapTextureSamplingParam(ws))
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, t.MapTextureSamplingParam(wt))
}
func (t *Texture_GL) SetPixelData(data []float32) {
	format := t.MapInternalFormat(Max(t.depth/4, 8))
	internalFormat := t.MapInternalFormat(Max(t.depth, 8))
	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1)
	gl.TexImage2D(gl.TEXTURE_2D, 0, int32(internalFormat), t.width, t.height, 0, uint32(format), gl.FLOAT, unsafe.Pointer(&data[0]))
}

func (t *Texture_GL) CopyData(src *Texture) {
	srcTex, ok := (*src).(*Texture_GL)
	if !ok || srcTex == nil || !srcTex.IsValid() || !t.IsValid() {
		return
	}
	if t.width != srcTex.width || t.height != srcTex.height || t.depth != srcTex.depth {
		return
	}
	gl.BindTexture(gl.TEXTURE_2D, srcTex.handle)
	// Allocate buffer for pixel data
	pixelSize := int(t.width) * int(t.height) * int(t.depth/8)
	data := make([]byte, pixelSize)
	gl.GetTexImage(gl.TEXTURE_2D, 0, srcTex.MapInternalFormat(Max(srcTex.depth, 8)), gl.UNSIGNED_BYTE, unsafe.Pointer(&data[0]))
	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.TexSubImage2D(gl.TEXTURE_2D, 0, 0, 0, t.width, t.height, t.MapInternalFormat(Max(t.depth, 8)), gl.UNSIGNED_BYTE, unsafe.Pointer(&data[0]))
}

// Return whether texture has a valid handle
func (t *Texture_GL) IsValid() bool {
	return t.width != 0 && t.height != 0 && t.handle != 0
}

func (t *Texture_GL) GetWidth() int32 {
	return t.width
}

func (t *Texture_GL) GetHeight() int32 {
	return t.height
}

func (t *Texture_GL) MapInternalFormat(i int32) uint32 {
	var InternalFormatLUT = map[int32]uint32{
		8:   gl.RED,
		24:  gl.RGB,
		32:  gl.RGBA,
		96:  gl.RGB32F,
		128: gl.RGBA32F,
	}
	return InternalFormatLUT[i]
}

func (t *Texture_GL) MapTextureSamplingParam(i TextureSamplingParam) int32 {
	var SamplingParam = map[TextureSamplingParam]int32{
		TextureSamplingFilterNearest:              gl.NEAREST,
		TextureSamplingFilterLinear:               gl.LINEAR,
		TextureSamplingFilterNearestMipMapNearest: gl.NEAREST_MIPMAP_NEAREST,
		TextureSamplingFilterLinearMipMapNearest:  gl.LINEAR_MIPMAP_NEAREST,
		TextureSamplingFilterNearestMipMapLinear:  gl.NEAREST_MIPMAP_LINEAR,
		TextureSamplingFilterLinearMipMapLinear:   gl.LINEAR_MIPMAP_LINEAR,
		TextureSamplingWrapClampToEdge:            gl.CLAMP_TO_EDGE,
		TextureSamplingWrapMirroredRepeat:         gl.MIRRORED_REPEAT,
		TextureSamplingWrapRepeat:                 gl.REPEAT,
	}
	return SamplingParam[i]
}

func (t *Texture_GL) SavePNG(filename string, pal []uint32) error {
	log.Printf("SavePNG: filename=%q width=%d height=%d depth=%d handle=%d",
		filename, t.width, t.height, t.depth, t.handle)

	if !t.IsValid() {
		log.Printf("SavePNG: texture is not valid (handle=%d, w=%d, h=%d)", t.handle, t.width, t.height)
		return Error("texture not valid")
	}

	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	// Allocate buffer for pixel data
	pixelSize := int(t.width) * int(t.height) * int(t.depth/8)
	data := make([]byte, pixelSize)
	gl.GetTexImage(gl.TEXTURE_2D, 0, t.MapInternalFormat(Max(t.depth, 8)), gl.UNSIGNED_BYTE, unsafe.Pointer(&data[0]))

	if len(data) != pixelSize {
		log.Printf("SavePNG: unexpected data length got=%d expected=%d", len(data), pixelSize)
	}

	if t.depth == 8 {
		// 8-bit texture: data contains palette indices. If a palette is provided
		// we'll create a paletted PNG using that palette. Otherwise fall back to
		// a grayscale image where palette indices map to gray levels.
		if pal != nil && len(pal) >= 256 {
			// Build Go palette from uint32 entries (A<<24 | B<<16 | G<<8 | R)
			gpal := make(color.Palette, 256)
			for i := 0; i < 256; i++ {
				c := pal[i]
				r := uint8(c & 0xff)
				g := uint8((c >> 8) & 0xff)
				b := uint8((c >> 16) & 0xff)
				a := uint8((c >> 24) & 0xff)
				gpal[i] = color.RGBA{R: r, G: g, B: b, A: a}
			}

			img := image.NewPaletted(image.Rect(0, 0, int(t.width), int(t.height)), gpal)
			n := copy(img.Pix, data)
			if n != len(img.Pix) {
				log.Printf("SavePNG: copied %d bytes into paletted image (expected %d)", n, len(img.Pix))
			}

			// Count used indices so we can make any used-but-fully-transparent
			// palette entries opaque for the paletted PNG output. This preserves
			// the index mapping while ensuring the saved paletted PNG is visible
			// in image viewers that honor palette alpha.
			usedCounts := make([]int, 256)
			for _, idx := range img.Pix {
				usedCounts[int(idx)]++
			}
			forced := []int{}
			for i := 0; i < 256; i++ {
				if usedCounts[i] > 0 {
					_, _, _, ca := gpal[i].RGBA()
					if ca>>8 == 0 { // alpha == 0
						// Replace palette entry with same RGB but full alpha so
						// paletted output is visible. Do not modify original
						// `pal` slice — operate on gpal copy.
						r, g, b, _ := gpal[i].RGBA()
						gpal[i] = color.RGBA{R: uint8(r >> 8), G: uint8(g >> 8), B: uint8(b >> 8), A: 255}
						forced = append(forced, i)
					}
				}
			}
			if len(forced) > 0 {
				log.Printf("SavePNG: forced %d palette entries to opaque for paletted output: %v", len(forced), forced)
			}

			f, err := os.Create(filename)
			if err != nil {
				return err
			}
			defer f.Close()

			if err := png.Encode(f, img); err != nil {
				log.Printf("SavePNG: png.Encode returned error: %v", err)
				return err
			}

			// Also save an expanded RGBA image so viewers see the atlas even if
			// the saved paletted PNG has transparent palette entries.
			expanded := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
			for i := range data {
				v := int(data[i])
				if v < len(gpal) {
					cr, cg, cb, ca := gpal[v].RGBA()
					expanded.Pix[4*i+0] = uint8(cr >> 8)
					expanded.Pix[4*i+1] = uint8(cg >> 8)
					expanded.Pix[4*i+2] = uint8(cb >> 8)
					expanded.Pix[4*i+3] = uint8(ca >> 8)
				} else {
					expanded.Pix[4*i+0] = 255
					expanded.Pix[4*i+1] = 0
					expanded.Pix[4*i+2] = 255
					expanded.Pix[4*i+3] = 255
				}
			}
			if ef, eerr := os.Create(filename + ".rgba.png"); eerr == nil {
				png.Encode(ef, expanded)
				ef.Close()
				log.Printf("SavePNG: expanded RGBA image written to %s", filename+".rgba.png")
			} else {
				log.Printf("SavePNG: failed to write expanded RGBA: %v", eerr)
			}

			// Basic verification: ensure data isn't all a single repeated index (likely blank)
			counts := make([]int, 256)
			for i := range data {
				counts[data[i]]++
			}
			var unique int
			for _, c := range counts {
				if c > 0 {
					unique++
				}
			}

			// Log top indices and palette alpha stats to help debug invisible/blank outputs
			topN := 8
			used := make([]bool, 256)
			for k := 0; k < topN; k++ {
				bestIdx := -1
				bestCount := 0
				for i, c := range counts {
					if used[i] || c == 0 {
						continue
					}
					if c > bestCount {
						bestCount = c
						bestIdx = i
					}
				}
				if bestIdx == -1 {
					break
				}
				used[bestIdx] = true
				cr, cg, cb, ca := gpal[bestIdx].RGBA()
				log.Printf("SavePNG: top[%d] idx=%d count=%d color=R=%d G=%d B=%d A=%d", k, bestIdx, bestCount, cr>>8, cg>>8, cb>>8, ca>>8)
			}

			// Count zero-alpha palette entries
			zeroAlpha := 0
			for i := 0; i < len(gpal); i++ {
				_, _, _, ca := gpal[i].RGBA()
				if ca>>8 == 0 {
					zeroAlpha++
				}
			}
			if zeroAlpha > 0 {
				log.Printf("SavePNG: palette has %d/%d entries with zero alpha", zeroAlpha, len(gpal))
			}

			// Produce an index-map visualisation (ignores palette alpha) so we can
			// see the index layout independent of palette transparency.
			idxImg := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
			for i := range data {
				idx := int(data[i])
				r := uint8((idx * 97) % 256)
				g := uint8((idx * 53) % 256)
				b := uint8((idx * 193) % 256)
				idxImg.Pix[4*i+0] = r
				idxImg.Pix[4*i+1] = g
				idxImg.Pix[4*i+2] = b
				idxImg.Pix[4*i+3] = 255
			}
			if df, derr := os.Create(filename + ".indexmap.png"); derr == nil {
				png.Encode(df, idxImg)
				df.Close()
				log.Printf("SavePNG: index map written to %s", filename+".indexmap.png")
			} else {
				log.Printf("SavePNG: failed to write index map: %v", derr)
			}

			// Also write a palette-visualisation where each index is mapped
			// through the actual palette, but force alpha=255 so colors are visible
			palvis := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
			for i := range data {
				v := int(data[i])
				if v < len(gpal) {
					cr, cg, cb, _ := gpal[v].RGBA()
					palvis.Pix[4*i+0] = uint8(cr >> 8)
					palvis.Pix[4*i+1] = uint8(cg >> 8)
					palvis.Pix[4*i+2] = uint8(cb >> 8)
					palvis.Pix[4*i+3] = 255
				} else {
					palvis.Pix[4*i+0] = 255
					palvis.Pix[4*i+1] = 0
					palvis.Pix[4*i+2] = 255
					palvis.Pix[4*i+3] = 255
				}
			}
			if df, derr := os.Create(filename + ".palvis.png"); derr == nil {
				png.Encode(df, palvis)
				df.Close()
				log.Printf("SavePNG: palette visualisation written to %s", filename+".palvis.png")
			}

			if unique <= 1 {
				// Find the dominant index
				dominantIdx := 0
				dominantCount := 0
				for i, c := range counts {
					if c > dominantCount {
						dominantIdx = i
						dominantCount = c
					}
				}
				log.Printf("SavePNG: saved paletted PNG but image appears uniform (unique indices=%d) dominantIdx=%d count=%d. Writing debug dump.", unique, dominantIdx, dominantCount)

				// Log the first palette entries to help understand what the repeated
				// index maps to (alpha may be zero).
				for i := 0; i < 16 && i < len(gpal); i++ {
					cr, cg, cb, ca := gpal[i].RGBA()
					log.Printf("SavePNG: pal[%02d] = R=%d G=%d B=%d A=%d", i, cr>>8, cg>>8, cb>>8, ca>>8)
				}
				dbg := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
				for i := range data {
					v := int(data[i])
					if v < len(gpal) {
						cr, cg, cb, ca := gpal[v].RGBA()
						dbg.Pix[4*i+0] = uint8(cr >> 8)
						dbg.Pix[4*i+1] = uint8(cg >> 8)
						dbg.Pix[4*i+2] = uint8(cb >> 8)
						a8 := uint8(ca >> 8)
						if a8 == 0 {
							a8 = 255
						}
						dbg.Pix[4*i+3] = a8
					} else {
						dbg.Pix[4*i+0] = 255
						dbg.Pix[4*i+1] = 0
						dbg.Pix[4*i+2] = 255
						dbg.Pix[4*i+3] = 255
					}
				}
				dfn := filename + ".debug.png"
				if df, derr := os.Create(dfn); derr == nil {
					png.Encode(df, dbg)
					df.Close()
					log.Printf("SavePNG: debug dump written to %s", dfn)
				} else {
					log.Printf("SavePNG: failed to write debug dump: %v", derr)
				}
			}

			return nil
		}

		// Fallback: expand indices into an opaque grayscale NRGBA
		grayImg := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
		for i := 0; i < len(data) && 4*i+3 < len(grayImg.Pix); i++ {
			v := data[i]
			grayImg.Pix[4*i+0] = v
			grayImg.Pix[4*i+1] = v
			grayImg.Pix[4*i+2] = v
			grayImg.Pix[4*i+3] = 255
		}
		f, err := os.Create(filename)
		if err != nil {
			return err
		}
		defer f.Close()

		if err := png.Encode(f, grayImg); err != nil {
			log.Printf("SavePNG: png.Encode(grayscale) returned error: %v", err)
			return err
		}

		// Quick verification for grayscale export
		zero := true
		for _, b := range grayImg.Pix {
			if b != 0 {
				zero = false
				break
			}
		}
		if zero {
			log.Printf("SavePNG: grayscale export appears entirely zero for %s (w=%d h=%d)", filename, t.width, t.height)
		}

		return nil
	}

	// non-paletted data (4 bytes per pixel expected)
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer f.Close()
	normalImg := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
	n := copy(normalImg.Pix, data)
	if n != len(normalImg.Pix) {
		log.Printf("SavePNG: copied %d/%d bytes into RGBA image for %s", n, len(normalImg.Pix), filename)
	}
	if err := png.Encode(f, normalImg); err != nil {
		log.Printf("SavePNG: png.Encode(rgba) returned error: %v", err)
		return err
	}

	// Verify image appears not-empty
	allZero := true
	for _, b := range normalImg.Pix {
		if b != 0 {
			allZero = false
			break
		}
	}
	if allZero {
		log.Printf("SavePNG: saved image %s appears entirely zero (w=%d h=%d).", filename, t.width, t.height)
		// create a visible diagnostic variant (invert) so the file is easier to inspect
		dbg := image.NewNRGBA(image.Rect(0, 0, int(t.width), int(t.height)))
		for i := 0; i < len(normalImg.Pix); i += 4 {
			dbg.Pix[i+0] = 255
			dbg.Pix[i+1] = 0
			dbg.Pix[i+2] = 0
			dbg.Pix[i+3] = 255
		}
		dfn := filename + ".debug.png"
		if df, derr := os.Create(dfn); derr == nil {
			png.Encode(df, dbg)
			df.Close()
			log.Printf("SavePNG: debug debug variant written to %s", dfn)
		}
	}

	return nil
}

// ------------------------------------------------------------------
// Renderer_GL

// (Renderer_GL type moved to top types section)
// (GLState declared in the top types section)

//go:embed shaders/sprite.vert.glsl
var vertShader string

//go:embed shaders/sprite.frag.glsl
var fragShader string

//go:embed shaders/model.vert.glsl
var modelVertShader string

//go:embed shaders/model.frag.glsl
var modelFragShader string

//go:embed shaders/shadow.vert.glsl
var shadowVertShader string

//go:embed shaders/shadow.frag.glsl
var shadowFragShader string

//go:embed shaders/shadow.geo.glsl
var shadowGeoShader string

//go:embed shaders/ident.vert.glsl
var identVertShader string

//go:embed shaders/ident.frag.glsl
var identFragShader string

//go:embed shaders/panoramaToCubeMap.frag.glsl
var panoramaToCubeMapFragShader string

//go:embed shaders/cubemapFiltering.frag.glsl
var cubemapFilteringFragShader string

func (r *Renderer_GL) GetName() string {
	return "OpenGL 3.3"
}

// init 3D model shader
func (r *Renderer_GL) InitModelShader() error {
	var err error
	if r.enableShadow {
		r.modelShader, err = r.newShaderProgram(modelVertShader, "#define ENABLE_SHADOW\n"+modelFragShader, "", "Model Shader", false)
	} else {
		r.modelShader, err = r.newShaderProgram(modelVertShader, modelFragShader, "", "Model Shader", false)
	}
	if err != nil {
		return err
	}
	r.modelShader.RegisterAttributes("vertexId", "position", "uv", "normalIn", "tangentIn", "vertColor", "joints_0", "joints_1", "weights_0", "weights_1", "outlineAttributeIn")
	r.modelShader.RegisterUniforms("model", "view", "projection", "normalMatrix", "unlit", "baseColorFactor", "add", "mult", "useTexture", "useNormalMap", "useMetallicRoughnessMap", "useEmissionMap", "neg", "gray", "hue",
		"enableAlpha", "alphaThreshold", "numJoints", "morphTargetWeight", "morphTargetOffset", "morphTargetTextureDimension", "numTargets", "numVertices",
		"metallicRoughness", "ambientOcclusionStrength", "emission", "environmentIntensity", "mipCount", "meshOutline",
		"cameraPosition", "environmentRotation", "texTransform", "normalMapTransform", "metallicRoughnessMapTransform", "ambientOcclusionMapTransform", "emissionMapTransform",
		"lightMatrices[0]", "lightMatrices[1]", "lightMatrices[2]", "lightMatrices[3]",
		"lights[0].direction", "lights[0].range", "lights[0].color", "lights[0].intensity", "lights[0].position", "lights[0].innerConeCos", "lights[0].outerConeCos", "lights[0].type", "lights[0].shadowBias", "lights[0].shadowMapFar",
		"lights[1].direction", "lights[1].range", "lights[1].color", "lights[1].intensity", "lights[1].position", "lights[1].innerConeCos", "lights[1].outerConeCos", "lights[1].type", "lights[1].shadowBias", "lights[1].shadowMapFar",
		"lights[2].direction", "lights[2].range", "lights[2].color", "lights[2].intensity", "lights[2].position", "lights[2].innerConeCos", "lights[2].outerConeCos", "lights[2].type", "lights[2].shadowBias", "lights[2].shadowMapFar",
		"lights[3].direction", "lights[3].range", "lights[3].color", "lights[3].intensity", "lights[3].position", "lights[3].innerConeCos", "lights[3].outerConeCos", "lights[3].type", "lights[3].shadowBias", "lights[3].shadowMapFar",
	)
	r.modelShader.RegisterTextures("tex", "morphTargetValues", "jointMatrices", "normalMap", "metallicRoughnessMap", "ambientOcclusionMap", "emissionMap", "lambertianEnvSampler", "GGXEnvSampler", "GGXLUT",
		"shadowCubeMap")

	if r.enableShadow {
		r.shadowMapShader, err = r.newShaderProgram(shadowVertShader, shadowFragShader, shadowGeoShader, "Shadow Map Shader", false)
		if err != nil {
			return err
		}
		r.shadowMapShader.RegisterAttributes("vertexId", "position", "vertColor", "uv", "joints_0", "joints_1", "weights_0", "weights_1")
		r.shadowMapShader.RegisterUniforms("model", "lightMatrices[0]", "lightMatrices[1]", "lightMatrices[2]", "lightMatrices[3]", "lightMatrices[4]", "lightMatrices[5]",
			"lightMatrices[6]", "lightMatrices[7]", "lightMatrices[8]", "lightMatrices[9]", "lightMatrices[10]", "lightMatrices[11]",
			"lightMatrices[12]", "lightMatrices[13]", "lightMatrices[14]", "lightMatrices[15]", "lightMatrices[16]", "lightMatrices[17]",
			"lightMatrices[18]", "lightMatrices[19]", "lightMatrices[20]", "lightMatrices[21]", "lightMatrices[22]", "lightMatrices[23]",
			"lights[0].type", "lights[1].type", "lights[2].type", "lights[3].type", "lights[0].position", "lights[1].position", "lights[2].position", "lights[3].position",
			"lights[0].shadowMapFar", "lights[1].shadowMapFar", "lights[2].shadowMapFar", "lights[3].shadowMapFar", "numJoints", "morphTargetWeight", "morphTargetOffset", "morphTargetTextureDimension",
			"numTargets", "numVertices", "enableAlpha", "alphaThreshold", "baseColorFactor", "useTexture", "texTransform", "layerOffset", "lightIndex")
		r.shadowMapShader.RegisterTextures("morphTargetValues", "jointMatrices", "tex")
	}
	r.panoramaToCubeMapShader, err = r.newShaderProgram(identVertShader, panoramaToCubeMapFragShader, "", "Panorama To Cubemap Shader", false)
	if err != nil {
		return err
	}
	r.panoramaToCubeMapShader.RegisterAttributes("VertCoord")
	r.panoramaToCubeMapShader.RegisterUniforms("currentFace")
	r.panoramaToCubeMapShader.RegisterTextures("panorama")

	r.cubemapFilteringShader, err = r.newShaderProgram(identVertShader, cubemapFilteringFragShader, "", "Cubemap Filtering Shader", false)
	if err != nil {
		return err
	}
	r.cubemapFilteringShader.RegisterAttributes("VertCoord")
	r.cubemapFilteringShader.RegisterUniforms("sampleCount", "distribution", "width", "currentFace", "roughness", "intensityScale", "isLUT")
	r.cubemapFilteringShader.RegisterTextures("cubeMap")
	return nil
}

// Render initialization.
// Creates the default shaders, the framebuffer and enables MSAA.
func (r *Renderer_GL) Init() {
	chk(gl.Init())
	fmt.Printf("Real GL Version: %v\n", gl.GoStr(gl.GetString(gl.VERSION)))
	fmt.Printf("Real GLSL Version: %v\n", gl.GoStr(gl.GetString(gl.SHADING_LANGUAGE_VERSION)))
	fmt.Printf("Real GL Renderer: %v\n", gl.GoStr(gl.GetString(gl.RENDERER)))
	fmt.Printf("Real GL Vendor: %v\n", gl.GoStr(gl.GetString(gl.VENDOR)))

	// var maxSamples int32
	// gl.GetIntegerv(gl.MAX_SAMPLES, &maxSamples)
	// if sys_msaa > maxSamples {
	// 	sys_cfg.SetValueUpdate("Video.MSAA", maxSamples)
	// 	sys_msaa = maxSamples
	// }

	// Store current timestamp
	sys_prevTimestamp = sys_GetTime()

	r.postShaderSelect = make([]*ShaderProgram_GL, 1+0)

	// Data buffers for rendering
	postVertData := f32.Bytes(binary.LittleEndian, -1, -1, 1, -1, -1, 1, 1, 1)

	r.enableModel = true   //sys_cfg.Video.EnableModel
	r.enableShadow = false //sys_cfg.Video.EnableModelShadow

	gl.GenVertexArrays(1, &r.vao)
	gl.BindVertexArray(r.vao)

	gl.GenBuffers(1, &r.postVertBuffer)

	gl.BindBuffer(gl.ARRAY_BUFFER, r.postVertBuffer)
	gl.BufferData(gl.ARRAY_BUFFER, len(postVertData), unsafe.Pointer(&postVertData[0]), gl.STATIC_DRAW)

	gl.GenBuffers(1, &r.vertexBuffer)
	gl.GenBuffers(2, &r.modelVertexBuffer[0])
	gl.GenBuffers(2, &r.modelIndexBuffer[0])

	// Sprite shader
	r.spriteShader, _ = r.newShaderProgram(vertShader, fragShader, "", "Main Shader", true)
	r.spriteShader.RegisterAttributes("position", "uv")
	r.spriteShader.RegisterUniforms("modelview", "projection", "x1x2x4x3",
		"alpha", "tint", "mask", "neg", "gray", "add", "mult", "isFlat", "isRgba", "isTrapez", "hue", "uvRect", "useUV")
	r.spriteShader.RegisterTextures("pal", "tex")

	if r.enableModel {
		if err := r.InitModelShader(); err != nil {
			r.enableModel = false
		}
	}

	// Compile postprocessing shaders

	// Calculate total amount of shaders loaded.
	r.postShaderSelect = make([]*ShaderProgram_GL, 1)

	// External Shaders
	// for i := 0; i < len(sys_cfg.Video.ExternalShaders); i++ {
	// 	r.postShaderSelect[i], _ = r.newShaderProgram(string(sys_externalShaders[0][i])+"\x00",
	// 		string(sys_externalShaders[1][i])+"\x00", "", fmt.Sprintf("Postprocess Shader #%v", i), true)
	// 	r.postShaderSelect[i].RegisterAttributes("VertCoord", "TexCoord")
	// 	loc := r.postShaderSelect[i].a["TexCoord"]
	// 	gl.VertexAttribPointer(uint32(loc), 3, gl.FLOAT, false, 5*4, gl.PtrOffset(2*4))
	// 	gl.EnableVertexAttribArray(uint32(loc))
	// 	r.postShaderSelect[i].RegisterUniforms("Texture_GL", "TextureSize", "CurrentTime")
	// }

	// Ident shader (no postprocessing). This is the last one
	identShader, _ := r.newShaderProgram(identVertShader, identFragShader, "", "Identity Postprocess", true)
	identShader.RegisterAttributes("VertCoord", "TexCoord")
	identShader.RegisterUniforms("Texture_GL", "TextureSize", "CurrentTime")
	r.postShaderSelect[len(r.postShaderSelect)-1] = identShader

	// if sys_msaa > 0 {
	// 	gl.Enable(gl.MULTISAMPLE)
	// }

	gl.ActiveTexture(gl.TEXTURE0)

	// create a texture for r.fbo
	gl.GenTextures(1, &r.fbo_texture)

	// if sys_msaa > 0 {
	// 	gl.BindTexture(gl.TEXTURE_2D_MULTISAMPLE, r.fbo_texture)
	// } else {
	gl.BindTexture(gl.TEXTURE_2D, r.fbo_texture)
	// }

	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)

	// Don't change this from gl.RGBA.
	// It breaks mixing between subtractive and additive.
	// if sys_msaa > 0 {
	// 	gl.TexImage2DMultisample(
	// 		gl.TEXTURE_2D_MULTISAMPLE,
	// 		sys_msaa,
	// 		gl.RGBA,
	// 		sys_scrrect[2],
	// 		sys_scrrect[3],
	// 		true,
	// 	)
	// } else {
	gl.TexImage2D(
		gl.TEXTURE_2D,
		0,
		gl.RGBA,
		sys_scrrect[2],
		sys_scrrect[3],
		0,
		gl.RGBA,
		gl.UNSIGNED_BYTE,
		nil,
	)
	// }

	r.fbo_pp = make([]uint32, 2)
	r.fbo_pp_texture = make([]uint32, 2)

	// Shaders might use negative values, so
	// we specify that we want signed pixels
	// r.fbo_pp_texture
	for i := 0; i < 2; i++ {
		gl.GenTextures(1, &(r.fbo_pp_texture[i]))
		gl.BindTexture(gl.TEXTURE_2D, r.fbo_pp_texture[i])
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
		gl.TexImage2D(
			gl.TEXTURE_2D,
			0,
			gl.RGBA8_SNORM,
			sys_scrrect[2],
			sys_scrrect[3],
			0,
			gl.RGBA,
			gl.UNSIGNED_BYTE,
			nil,
		)
	}

	// done with r.fbo_texture, unbind it
	gl.BindTexture(gl.TEXTURE_2D, 0)

	//r.rbo_depth = gl.CreateRenderbuffer()
	gl.GenRenderbuffers(1, &r.rbo_depth)

	gl.BindRenderbuffer(gl.RENDERBUFFER, r.rbo_depth)
	// if sys_msaa > 0 {
	//gl.RenderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, int(sys_scrrect[2]), int(sys_scrrect[3]))
	// gl.RenderbufferStorageMultisample(gl.RENDERBUFFER, sys_msaa, gl.DEPTH_COMPONENT16, sys_scrrect[2], sys_scrrect[3])
	// } else {
	gl.RenderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, sys_scrrect[2], sys_scrrect[3])
	// }
	gl.BindRenderbuffer(gl.RENDERBUFFER, 0)
	// if sys_msaa > 0 {
	// r.fbo_f_texture = r.newTexture(sys_scrrect[2], sys_scrrect[3], 32, false).(*Texture_GL)
	// r.fbo_f_texture.SetData(nil)
	// } else {
	//r.rbo_depth = gl.CreateRenderbuffer()
	//gl.BindRenderbuffer(gl.RENDERBUFFER, r.rbo_depth)
	//gl.RenderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, int(sys_scrrect[2]), int(sys_scrrect[3]))
	//gl.BindRenderbuffer(gl.RENDERBUFFER, gl.NoRenderbuffer)
	// }

	// create an FBO for our r.fbo, which is then for r.fbo_texture
	gl.GenFramebuffers(1, &r.fbo)
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo)

	// if sys_msaa > 0 {
	// 	gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D_MULTISAMPLE, r.fbo_texture, 0)
	// 	gl.FramebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, r.rbo_depth)
	// 	if status := gl.CheckFramebufferStatus(gl.FRAMEBUFFER); status != gl.FRAMEBUFFER_COMPLETE {
	// 		sys_errLog.Printf("framebuffer create failed: 0x%x", status)
	// 		fmt.Printf("framebuffer create failed: 0x%x \n", status)
	// 	}
	// 	gl.GenFramebuffers(1, &r.fbo_f)
	// 	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_f)
	// 	gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, r.fbo_f_texture.handle, 0)
	// } else {
	gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, r.fbo_texture, 0)
	gl.FramebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, r.rbo_depth)
	// }

	// create our two FBOs for our postprocessing needs
	for i := 0; i < 2; i++ {
		gl.GenFramebuffers(1, &(r.fbo_pp[i]))
		gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_pp[i])
		gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, r.fbo_pp_texture[i], 0)
	}

	// create an FBO for our model stuff
	if r.enableModel {
		if r.enableShadow {
			gl.GenFramebuffers(1, &r.fbo_shadow)
			gl.ActiveTexture(gl.TEXTURE0)
			gl.GenTextures(1, &r.fbo_shadow_cube_texture)

			gl.BindTexture(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, r.fbo_shadow_cube_texture)
			gl.TexStorage3D(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, 1, gl.DEPTH_COMPONENT24, 1024, 1024, 4*6)
			gl.TexParameteri(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
			gl.TexParameteri(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
			gl.TexParameteri(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
			gl.TexParameteri(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)

			gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_shadow)
			gl.DrawBuffer(gl.NONE)
			sys_nDrawcall++
			gl.ReadBuffer(gl.NONE)
			if status := gl.CheckFramebufferStatus(gl.FRAMEBUFFER); status != gl.FRAMEBUFFER_COMPLETE {
				fmt.Printf("framebuffer create failed: 0x%x", status)
			}
		}
		gl.GenFramebuffers(1, &r.fbo_env)
	}
	gl.BindFramebuffer(gl.FRAMEBUFFER, 0)
}

func (r *Renderer_GL) Close() {
}

func (r *Renderer_GL) IsModelEnabled() bool {
	return r.enableModel
}

func (r *Renderer_GL) IsShadowEnabled() bool {
	return r.enableShadow
}

func (r *Renderer_GL) BeginFrame(clearColor bool) {
	sys_absTickCountF++
	gl.BindVertexArray(r.vao)
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo)
	gl.Viewport(0, 0, sys_scrrect[2], sys_scrrect[3])
	if clearColor {
		gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
	} else {
		gl.Clear(gl.DEPTH_BUFFER_BIT)
	}
}

func (r *Renderer_GL) BlendReset() {
	gl.BlendEquation(r.MapBlendEquation(BlendAdd))
	gl.BlendFunc(r.MapBlendFunction(BlendSrcAlpha), r.MapBlendFunction(BlendOneMinusSrcAlpha))
}
func (r *Renderer_GL) EndFrame() {
	if len(r.fbo_pp) == 0 {
		return
	}
	// tell GL to use our vertex array object
	// this'll be where our quad is stored
	gl.BindVertexArray(r.vao)

	x, y, width, height := int32(0), int32(0), int32(sys_scrrect[2]), int32(sys_scrrect[3])
	time := sys_GetTime() // consistent time across all shaders

	// if sys_msaa > 0 {
	// 	gl.BindFramebuffer(gl.DRAW_FRAMEBUFFER, r.fbo_f)
	// 	gl.BindFramebuffer(gl.READ_FRAMEBUFFER, r.fbo)
	// 	gl.BlitFramebuffer(x, y, width, height, x, y, width, height, gl.COLOR_BUFFER_BIT, gl.LINEAR)
	// }

	var scaleMode int32 // GL enum
	// if sys_cfg.Video.WindowScaleMode {
	scaleMode = gl.LINEAR
	// } else {
	// scaleMode = gl.NEAREST
	// }

	// set the viewport to the unscaled bounds for post-processing
	gl.Viewport(x, y, width, height)
	// clear both of our post-processing FBOs to make sure
	// nothing's there. the output is set later
	for i := 0; i < 2; i++ {
		gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_pp[i])
		gl.Clear(gl.COLOR_BUFFER_BIT)
	}
	gl.ActiveTexture(gl.TEXTURE0) // later referred to by Texture_GL

	fbo_texture := r.fbo_texture
	// if sys_msaa > 0 {
	// 	fbo_texture = r.fbo_f_texture.handle
	// }

	// disable blending
	gl.Disable(gl.BLEND)

	for i := 0; i < len(r.postShaderSelect); i++ {
		postShader := r.postShaderSelect[i]

		// this is here because it is undefined
		// behavior to write to the same FBO
		if i%2 == 0 {
			// ping! our first post-processing FBO is the output
			gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_pp[0])
			if i == 0 {
				// first pass, use fbo_texture
				gl.BindTexture(gl.TEXTURE_2D, fbo_texture)
			} else {
				// not the first pass, use the second post-processing FBO
				gl.BindTexture(gl.TEXTURE_2D, r.fbo_pp_texture[1])
			}
		} else {
			// pong! our second post-processing FBO is the output
			gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_pp[1])
			// our first post-processing FBO is the input
			gl.BindTexture(gl.TEXTURE_2D, r.fbo_pp_texture[0])
		}

		if i >= len(r.postShaderSelect)-1 {
			// this is the last shader,
			// so we ask GL to scale it and output it
			// to FB0, the default frame buffer that the user sees
			x, y, width, height := GetScaledViewportSize()
			gl.Viewport(x, y, width, height)
			gl.BindFramebuffer(gl.FRAMEBUFFER, 0)
			// clear FB0 just to make sure
			gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
		}

		// tell GL we want to use our shader program
		gl.UseProgram(postShader.program)

		// set post-processing parameters
		gl.Uniform1i(postShader.u["Texture_GL"], 0)
		gl.Uniform2f(postShader.u["TextureSize"], float32(width), float32(height))
		gl.Uniform1f(postShader.u["CurrentTime"], float32(time))
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, scaleMode)
		gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, scaleMode)

		// this actually draws the image to the FBO
		// by constructing a quad (2 tris)
		gl.BindBuffer(gl.ARRAY_BUFFER, r.postVertBuffer)

		// construct the UVs of the quad
		loc := postShader.a["VertCoord"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointer(uint32(loc), 2, gl.FLOAT, false, 0, nil)

		// construct the quad and draw it
		gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4)
		sys_nDrawcall++
		gl.DisableVertexAttribArray(uint32(loc))
	}
	sys_Drawcall = sys_nDrawcall
	sys_nDrawcall = 0
}

func (r *Renderer_GL) Await() {
	gl.Finish()
}

func (r *Renderer_GL) MapBlendEquation(i BlendEquation) uint32 {
	var BlendEquationLUT = map[BlendEquation]uint32{
		BlendAdd:             gl.FUNC_ADD,
		BlendReverseSubtract: gl.FUNC_REVERSE_SUBTRACT,
	}
	return BlendEquationLUT[i]
}

func (r *Renderer_GL) MapBlendFunction(i BlendFunc) uint32 {
	var BlendFunctionLUT = map[BlendFunc]uint32{
		BlendOne:              gl.ONE,
		BlendZero:             gl.ZERO,
		BlendSrcAlpha:         gl.SRC_ALPHA,
		BlendOneMinusSrcAlpha: gl.ONE_MINUS_SRC_ALPHA,
		BlendOneMinusDstColor: gl.ONE_MINUS_DST_COLOR,
		BlendDstColor:         gl.DST_COLOR,
	}
	return BlendFunctionLUT[i]
}

func (r *Renderer_GL) MapPrimitiveMode(i PrimitiveMode) uint32 {
	var PrimitiveModeLUT = map[PrimitiveMode]uint32{
		LINES:          gl.LINES,
		LINE_LOOP:      gl.LINE_LOOP,
		LINE_STRIP:     gl.LINE_STRIP,
		TRIANGLES:      gl.TRIANGLES,
		TRIANGLE_STRIP: gl.TRIANGLE_STRIP,
		TRIANGLE_FAN:   gl.TRIANGLE_FAN,
	}
	return PrimitiveModeLUT[i]
}

func (r *Renderer_GL) SetDepthTest(depthTest bool) {
	if depthTest != r.depthTest {
		r.depthTest = depthTest
		if depthTest {
			gl.Enable(gl.DEPTH_TEST)
			gl.DepthFunc(gl.LESS)
		} else {
			gl.Disable(gl.DEPTH_TEST)
		}
	}
}

func (r *Renderer_GL) SetDepthMask(depthMask bool) {
	if depthMask != r.depthMask {
		r.depthMask = depthMask
		gl.DepthMask(depthMask)
	}
}

func (r *Renderer_GL) SetFrontFace(invertFrontFace bool) {
	if invertFrontFace != r.invertFrontFace {
		r.invertFrontFace = invertFrontFace
		if invertFrontFace {
			gl.FrontFace(gl.CW)
		} else {
			gl.FrontFace(gl.CCW)
		}
	}
}
func (r *Renderer_GL) SetCullFace(doubleSided bool) {
	if doubleSided != r.doubleSided {
		r.doubleSided = doubleSided
		if !doubleSided {
			gl.Enable(gl.CULL_FACE)
			gl.CullFace(gl.BACK)
		} else {
			gl.Disable(gl.CULL_FACE)
		}
	}
}
func (r *Renderer_GL) SetBlending(eq BlendEquation, src, dst BlendFunc) {
	if eq != r.blendEquation {
		r.blendEquation = eq
		gl.BlendEquation(r.MapBlendEquation(eq))
	}
	if src != r.blendSrc || dst != r.blendDst {
		r.blendSrc = src
		r.blendDst = dst
		gl.BlendFunc(r.MapBlendFunction(src), r.MapBlendFunction(dst))
	}
}

func (r *Renderer_GL) SetPipeline(eq BlendEquation, src, dst BlendFunc) {
	gl.BindVertexArray(r.vao)
	gl.UseProgram(r.spriteShader.program)

	gl.BlendEquation(r.MapBlendEquation(eq))
	gl.BlendFunc(r.MapBlendFunction(src), r.MapBlendFunction(dst))
	gl.Enable(gl.BLEND)

	// Must bind buffer before enabling attributes
	gl.BindBuffer(gl.ARRAY_BUFFER, r.vertexBuffer)
	loc := r.spriteShader.a["position"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 16, 0)
	loc = r.spriteShader.a["uv"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 16, 8)
}

func (r *Renderer_GL) ReleasePipeline() {
	loc := r.spriteShader.a["position"]
	gl.DisableVertexAttribArray(uint32(loc))
	loc = r.spriteShader.a["uv"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.Disable(gl.BLEND)
}

func (r *Renderer_GL) prepareShadowMapPipeline(bufferIndex uint32) {
	gl.UseProgram(r.shadowMapShader.program)
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_shadow)
	gl.Viewport(0, 0, 1024, 1024)
	gl.Enable(gl.TEXTURE_2D)
	gl.Disable(gl.BLEND)
	gl.Enable(gl.DEPTH_TEST)
	gl.DepthFunc(gl.LESS)
	gl.DepthMask(true)
	gl.BlendEquation(gl.FUNC_ADD)
	gl.BlendFunc(gl.ONE, gl.ZERO)
	if r.invertFrontFace {
		gl.FrontFace(gl.CW)
	} else {
		gl.FrontFace(gl.CCW)
	}
	if !r.doubleSided {
		gl.Enable(gl.CULL_FACE)
		gl.CullFace(gl.BACK)
	} else {
		gl.Disable(gl.CULL_FACE)
	}
	r.depthTest = true
	r.depthMask = true
	r.blendEquation = BlendAdd
	r.blendSrc = BlendOne
	r.blendDst = BlendZero

	gl.BindBuffer(gl.ARRAY_BUFFER, r.modelVertexBuffer[bufferIndex])
	gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, r.modelIndexBuffer[bufferIndex])

	gl.FramebufferTexture(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, r.fbo_shadow_cube_texture, 0)
	gl.Clear(gl.DEPTH_BUFFER_BIT)
}
func (r *Renderer_GL) setShadowMapPipeline(doubleSided, invertFrontFace, useUV, useNormal, useTangent, useVertColor, useJoint0, useJoint1 bool, numVertices, vertAttrOffset uint32) {
	r.SetFrontFace(invertFrontFace)
	r.SetCullFace(doubleSided)

	loc := r.shadowMapShader.a["vertexId"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 1, gl.INT, false, 0, uintptr(vertAttrOffset))
	offset := vertAttrOffset + 4*numVertices

	loc = r.shadowMapShader.a["position"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 3, gl.FLOAT, false, 0, uintptr(offset))
	offset += 12 * numVertices
	if useUV {
		r.useUV = true
		loc = r.modelShader.a["uv"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 0, uintptr(offset))
		offset += 8 * numVertices
	} else if r.useUV {
		r.useUV = false
		loc = r.modelShader.a["uv"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib2f(uint32(loc), 0, 0)
	}
	if useNormal {
		offset += 12 * numVertices
	}
	if useTangent {
		offset += 16 * numVertices
	}
	if useVertColor {
		loc = r.shadowMapShader.a["vertColor"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
	} else {
		loc = r.shadowMapShader.a["vertColor"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 1, 1, 1, 1)
	}
	if useJoint0 {
		r.useJoint0 = true
		loc = r.modelShader.a["joints_0"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
		loc = r.modelShader.a["weights_0"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
		if useJoint1 {
			r.useJoint1 = true
			loc = r.modelShader.a["joints_1"]
			gl.EnableVertexAttribArray(uint32(loc))
			gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
			offset += 16 * numVertices
			loc = r.modelShader.a["weights_1"]
			gl.EnableVertexAttribArray(uint32(loc))
			gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
			offset += 16 * numVertices
		} else if r.useJoint1 {
			r.useJoint1 = false
			loc = r.modelShader.a["joints_1"]
			gl.DisableVertexAttribArray(uint32(loc))
			gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
			loc = r.modelShader.a["weights_1"]
			gl.DisableVertexAttribArray(uint32(loc))
			gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		}
	} else if r.useJoint0 {
		r.useJoint0 = false
		r.useJoint1 = false
		loc = r.modelShader.a["joints_0"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		loc = r.modelShader.a["weights_0"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		loc = r.modelShader.a["joints_1"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		loc = r.modelShader.a["weights_1"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	}
}

func (r *Renderer_GL) ReleaseShadowPipeline() {
	loc := r.modelShader.a["vertexId"]
	gl.DisableVertexAttribArray(uint32(loc))
	loc = r.modelShader.a["position"]
	gl.DisableVertexAttribArray(uint32(loc))
	loc = r.modelShader.a["uv"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib2f(uint32(loc), 0, 0)
	loc = r.modelShader.a["vertColor"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 1, 1, 1, 1)
	loc = r.modelShader.a["joints_0"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["weights_0"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["joints_1"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["weights_1"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	//gl.Disable(gl.TEXTURE_2D)
	gl.DepthMask(true)
	gl.Disable(gl.DEPTH_TEST)
	gl.Disable(gl.CULL_FACE)
	gl.Disable(gl.BLEND)
	r.useUV = false
	r.useJoint0 = false
	r.useJoint1 = false
}

// func (r *Renderer_GL) prepareModelPipeline(bufferIndex uint32, env *Environment) {
// 	gl.UseProgram(r.modelShader.program)
// 	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo)
// 	gl.Viewport(0, 0, sys_scrrect[2], sys_scrrect[3])
// 	gl.Clear(gl.DEPTH_BUFFER_BIT)
// 	gl.Enable(gl.TEXTURE_2D)
// 	gl.Enable(gl.TEXTURE_CUBE_MAP)
// 	gl.Enable(gl.BLEND)

// 	if r.depthTest {
// 		gl.Enable(gl.DEPTH_TEST)
// 		gl.DepthFunc(gl.LESS)
// 	} else {
// 		gl.Disable(gl.DEPTH_TEST)
// 	}
// 	gl.DepthMask(r.depthMask)
// 	if r.invertFrontFace {
// 		gl.FrontFace(gl.CW)
// 	} else {
// 		gl.FrontFace(gl.CCW)
// 	}
// 	if !r.doubleSided {
// 		gl.Enable(gl.CULL_FACE)
// 		gl.CullFace(gl.BACK)
// 	} else {
// 		gl.Disable(gl.CULL_FACE)
// 	}
// 	gl.BlendEquation(r.MapBlendEquation(r.blendEquation))
// 	gl.BlendFunc(r.MapBlendFunction(r.blendSrc), r.MapBlendFunction(r.blendDst))

// 	gl.BindBuffer(gl.ARRAY_BUFFER, r.modelVertexBuffer[bufferIndex])
// 	gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, r.modelIndexBuffer[bufferIndex])
// 	if r.enableShadow {
// 		loc, unit := r.modelShader.u["shadowCubeMap"], r.modelShader.t["shadowCubeMap"]
// 		gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
// 		gl.BindTexture(gl.TEXTURE_CUBE_MAP_ARRAY_ARB, r.fbo_shadow_cube_texture)
// 		gl.Uniform1i(loc, int32(unit))
// 	}
// 	if env != nil {
// 		loc, unit := r.modelShader.u["lambertianEnvSampler"], r.modelShader.t["lambertianEnvSampler"]
// 		gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
// 		gl.BindTexture(gl.TEXTURE_CUBE_MAP, env.lambertianTexture.tex.(*Texture_GL).handle)
// 		gl.Uniform1i(loc, int32(unit))
// 		loc, unit = r.modelShader.u["GGXEnvSampler"], r.modelShader.t["GGXEnvSampler"]
// 		gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
// 		gl.BindTexture(gl.TEXTURE_CUBE_MAP, env.GGXTexture.tex.(*Texture_GL).handle)
// 		gl.Uniform1i(loc, int32(unit))
// 		loc, unit = r.modelShader.u["GGXLUT"], r.modelShader.t["GGXLUT"]
// 		gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
// 		gl.BindTexture(gl.TEXTURE_2D, env.GGXLUT.tex.(*Texture_GL).handle)
// 		gl.Uniform1i(loc, int32(unit))

// 		loc = r.modelShader.u["environmentIntensity"]
// 		gl.Uniform1f(loc, env.environmentIntensity)
// 		loc = r.modelShader.u["mipCount"]
// 		gl.Uniform1i(loc, env.mipmapLevels)
// 		loc = r.modelShader.u["environmentRotation"]
// 		rotationMatrix := mgl.Rotate3DX(math.Pi).Mul3(mgl.Rotate3DY(0.5 * math.Pi))
// 		rotationM := rotationMatrix[:]
// 		gl.UniformMatrix3fv(loc, 1, false, &rotationM[0])

//		} else {
//			loc, unit := r.modelShader.u["lambertianEnvSampler"], r.modelShader.t["lambertianEnvSampler"]
//			gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
//			gl.BindTexture(gl.TEXTURE_CUBE_MAP, 0)
//			gl.Uniform1i(loc, int32(unit))
//			loc, unit = r.modelShader.u["GGXEnvSampler"], r.modelShader.t["GGXEnvSampler"]
//			gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
//			gl.BindTexture(gl.TEXTURE_CUBE_MAP, 0)
//			gl.Uniform1i(loc, int32(unit))
//			loc, unit = r.modelShader.u["GGXLUT"], r.modelShader.t["GGXLUT"]
//			gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
//			gl.BindTexture(gl.TEXTURE_2D, 0)
//			gl.Uniform1i(loc, int32(unit))
//			loc = r.modelShader.u["environmentIntensity"]
//			gl.Uniform1f(loc, 0)
//		}
//	}
func (r *Renderer_GL) SetModelPipeline(eq BlendEquation, src, dst BlendFunc, depthTest, depthMask, doubleSided, invertFrontFace, useUV, useNormal, useTangent, useVertColor, useJoint0, useJoint1, useOutlineAttribute bool, numVertices, vertAttrOffset uint32) {
	r.SetDepthTest(depthTest)
	r.SetDepthMask(depthMask)
	r.SetFrontFace(invertFrontFace)
	r.SetCullFace(doubleSided)
	r.SetBlending(eq, src, dst)

	loc := r.modelShader.a["vertexId"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 1, gl.INT, false, 0, uintptr(vertAttrOffset))
	offset := vertAttrOffset + 4*numVertices

	loc = r.modelShader.a["position"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 3, gl.FLOAT, false, 0, uintptr(offset))
	offset += 12 * numVertices
	if useUV {
		r.useUV = true
		loc = r.modelShader.a["uv"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 0, uintptr(offset))
		offset += 8 * numVertices
	} else if r.useUV {
		r.useUV = false
		loc = r.modelShader.a["uv"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib2f(uint32(loc), 0, 0)
	}
	if useNormal {
		r.useNormal = true
		loc = r.modelShader.a["normalIn"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 3, gl.FLOAT, false, 0, uintptr(offset))
		offset += 12 * numVertices
	} else if r.useNormal {
		r.useNormal = false
		loc = r.modelShader.a["normalIn"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib3f(uint32(loc), 0, 0, 0)
	}
	if useTangent {
		r.useTangent = true
		loc = r.modelShader.a["tangentIn"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
	} else if r.useTangent {
		r.useTangent = false
		loc = r.modelShader.a["tangentIn"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	}
	if useVertColor {
		r.useVertColor = true
		loc = r.modelShader.a["vertColor"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
	} else if r.useVertColor {
		r.useVertColor = false
		loc = r.modelShader.a["vertColor"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 1, 1, 1, 1)
	}
	if useJoint0 {
		r.useJoint0 = true
		loc = r.modelShader.a["joints_0"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
		loc = r.modelShader.a["weights_0"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
		if useJoint1 {
			r.useJoint1 = true
			loc = r.modelShader.a["joints_1"]
			gl.EnableVertexAttribArray(uint32(loc))
			gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
			offset += 16 * numVertices
			loc = r.modelShader.a["weights_1"]
			gl.EnableVertexAttribArray(uint32(loc))
			gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
			offset += 16 * numVertices
		} else if r.useJoint1 {
			r.useJoint1 = false
			loc = r.modelShader.a["joints_1"]
			gl.DisableVertexAttribArray(uint32(loc))
			gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
			loc = r.modelShader.a["weights_1"]
			gl.DisableVertexAttribArray(uint32(loc))
			gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		}
	} else if r.useJoint0 {
		r.useJoint0 = false
		r.useJoint1 = false
		loc = r.modelShader.a["joints_0"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		loc = r.modelShader.a["weights_0"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		loc = r.modelShader.a["joints_1"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
		loc = r.modelShader.a["weights_1"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	}
	if useOutlineAttribute {
		r.useOutlineAttribute = true
		loc = r.modelShader.a["outlineAttributeIn"]
		gl.EnableVertexAttribArray(uint32(loc))
		gl.VertexAttribPointerWithOffset(uint32(loc), 4, gl.FLOAT, false, 0, uintptr(offset))
		offset += 16 * numVertices
	} else if r.useOutlineAttribute {
		r.useOutlineAttribute = false
		loc = r.modelShader.a["outlineAttributeIn"]
		gl.DisableVertexAttribArray(uint32(loc))
		gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	}
}
func (r *Renderer_GL) SetMeshOulinePipeline(invertFrontFace bool, meshOutline float32) {
	r.SetFrontFace(invertFrontFace)
	r.SetDepthTest(true)
	r.SetDepthMask(true)

	loc := r.modelShader.u["meshOutline"]
	gl.Uniform1f(loc, meshOutline)
}
func (r *Renderer_GL) ReleaseModelPipeline() {
	loc := r.modelShader.a["vertexId"]
	gl.DisableVertexAttribArray(uint32(loc))
	loc = r.modelShader.a["position"]
	gl.DisableVertexAttribArray(uint32(loc))
	loc = r.modelShader.a["uv"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib2f(uint32(loc), 0, 0)
	loc = r.modelShader.a["normalIn"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib3f(uint32(loc), 0, 0, 0)
	loc = r.modelShader.a["tangentIn"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["vertColor"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 1, 1, 1, 1)
	loc = r.modelShader.a["joints_0"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["weights_0"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["joints_1"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["weights_1"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	loc = r.modelShader.a["outlineAttributeIn"]
	gl.DisableVertexAttribArray(uint32(loc))
	gl.VertexAttrib4f(uint32(loc), 0, 0, 0, 0)
	//gl.Disable(gl.TEXTURE_2D)
	gl.DepthMask(true)
	gl.Disable(gl.DEPTH_TEST)
	gl.Disable(gl.CULL_FACE)
	r.useUV = false
	r.useNormal = false
	r.useTangent = false
	r.useVertColor = false
	r.useJoint0 = false
	r.useJoint1 = false
	r.useOutlineAttribute = false
}

func (r *Renderer_GL) ReadPixels(data []uint8, width, height int) {
	// we defer the EndFrame(), SwapBuffers(), and BeginFrame() calls that were previously below now to
	// a single spot in order to prevent the blank screenshot bug on single digit FPS
	gl.BindFramebuffer(gl.READ_FRAMEBUFFER, 0)
	gl.ReadPixels(0, 0, int32(width), int32(height), gl.RGBA, gl.UNSIGNED_BYTE, unsafe.Pointer(&data[0]))
}

func (r *Renderer_GL) Scissor(x, y, width, height int32) {
	gl.Enable(gl.SCISSOR_TEST)
	gl.Scissor(x, sys_scrrect[3]-(y+height), width, height)
}

func (r *Renderer_GL) DisableScissor() {
	gl.Disable(gl.SCISSOR_TEST)
}

func (r *Renderer_GL) SetUniformI(name string, val int) {
	loc := r.spriteShader.u[name]
	gl.Uniform1i(loc, int32(val))
}

func (r *Renderer_GL) SetUniformF(name string, values ...float32) {
	loc := r.spriteShader.u[name]
	switch len(values) {
	case 1:
		gl.Uniform1f(loc, values[0])
	case 2:
		gl.Uniform2f(loc, values[0], values[1])
	case 3:
		gl.Uniform3f(loc, values[0], values[1], values[2])
	case 4:
		gl.Uniform4f(loc, values[0], values[1], values[2], values[3])
	}
}

func (r *Renderer_GL) SetUniformFv(name string, values []float32) {
	loc := r.spriteShader.u[name]
	switch len(values) {
	case 2:
		gl.Uniform2fv(loc, 1, &values[0])
	case 3:
		gl.Uniform3fv(loc, 1, &values[0])
	case 4:
		gl.Uniform4fv(loc, 1, &values[0])
	}
}

func (r *Renderer_GL) SetUniformMatrix(name string, value []float32) {
	loc := r.spriteShader.u[name]
	gl.UniformMatrix4fv(loc, 1, false, &value[0])
}

func (r *Renderer_GL) SetTexture(name string, tex Texture) {
	t := tex.(*Texture_GL)
	loc, unit := r.spriteShader.u[name], r.spriteShader.t[name]
	gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.Uniform1i(loc, int32(unit))
}

func (r *Renderer_GL) SetModelUniformI(name string, val int) {
	loc := r.modelShader.u[name]
	gl.Uniform1i(loc, int32(val))
}

func (r *Renderer_GL) SetModelUniformF(name string, values ...float32) {
	loc := r.modelShader.u[name]
	switch len(values) {
	case 1:
		gl.Uniform1f(loc, values[0])
	case 2:
		gl.Uniform2f(loc, values[0], values[1])
	case 3:
		gl.Uniform3f(loc, values[0], values[1], values[2])
	case 4:
		gl.Uniform4f(loc, values[0], values[1], values[2], values[3])
	}
}
func (r *Renderer_GL) SetModelUniformFv(name string, values []float32) {
	loc := r.modelShader.u[name]
	switch len(values) {
	case 2:
		gl.Uniform2fv(loc, 1, &values[0])
	case 3:
		gl.Uniform3fv(loc, 1, &values[0])
	case 4:
		gl.Uniform4fv(loc, 1, &values[0])
	case 8:
		gl.Uniform4fv(loc, 2, &values[0])
	}
}
func (r *Renderer_GL) SetModelUniformMatrix(name string, value []float32) {
	loc := r.modelShader.u[name]
	gl.UniformMatrix4fv(loc, 1, false, &value[0])
}

func (r *Renderer_GL) SetModelUniformMatrix3(name string, value []float32) {
	loc := r.modelShader.u[name]
	gl.UniformMatrix3fv(loc, 1, false, &value[0])
}

func (r *Renderer_GL) SetModelTexture(name string, tex Texture) {
	t := tex.(*Texture_GL)
	loc, unit := r.modelShader.u[name], r.modelShader.t[name]
	gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.Uniform1i(loc, int32(unit))
}

func (r *Renderer_GL) SetShadowMapUniformI(name string, val int) {
	loc := r.shadowMapShader.u[name]
	gl.Uniform1i(loc, int32(val))
}

func (r *Renderer_GL) SetShadowMapUniformF(name string, values ...float32) {
	loc := r.shadowMapShader.u[name]
	switch len(values) {
	case 1:
		gl.Uniform1f(loc, values[0])
	case 2:
		gl.Uniform2f(loc, values[0], values[1])
	case 3:
		gl.Uniform3f(loc, values[0], values[1], values[2])
	case 4:
		gl.Uniform4f(loc, values[0], values[1], values[2], values[3])
	}
}
func (r *Renderer_GL) SetShadowMapUniformFv(name string, values []float32) {
	loc := r.shadowMapShader.u[name]
	switch len(values) {
	case 2:
		gl.Uniform2fv(loc, 1, &values[0])
	case 3:
		gl.Uniform3fv(loc, 1, &values[0])
	case 4:
		gl.Uniform4fv(loc, 1, &values[0])
	case 8:
		gl.Uniform4fv(loc, 2, &values[0])
	}
}
func (r *Renderer_GL) SetShadowMapUniformMatrix(name string, value []float32) {
	loc := r.shadowMapShader.u[name]
	gl.UniformMatrix4fv(loc, 1, false, &value[0])
}

func (r *Renderer_GL) SetShadowMapUniformMatrix3(name string, value []float32) {
	loc := r.shadowMapShader.u[name]
	gl.UniformMatrix3fv(loc, 1, false, &value[0])
}

func (r *Renderer_GL) SetShadowMapTexture(name string, tex Texture) {
	t := tex.(*Texture_GL)
	loc, unit := r.shadowMapShader.u[name], r.shadowMapShader.t[name]
	gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
	gl.BindTexture(gl.TEXTURE_2D, t.handle)
	gl.Uniform1i(loc, int32(unit))
}

func (r *Renderer_GL) SetShadowFrameTexture(i uint32) {
	gl.FramebufferTexture(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, r.fbo_shadow_cube_texture, 0)
}

func (r *Renderer_GL) SetShadowFrameCubeTexture(i uint32) {
	gl.FramebufferTexture(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, r.fbo_shadow_cube_texture, 0)
}

func (r *Renderer_GL) SetVertexData(values ...float32) {
	data := f32.Bytes(binary.LittleEndian, values...)
	gl.BindBuffer(gl.ARRAY_BUFFER, r.vertexBuffer)
	gl.BufferData(gl.ARRAY_BUFFER, len(data), unsafe.Pointer(&data[0]), gl.STATIC_DRAW)
}
func (r *Renderer_GL) SetVertexData2(values []float32) {
	data := f32.Bytes(binary.LittleEndian, values...)
	gl.BindBuffer(gl.ARRAY_BUFFER, r.vertexBuffer)
	gl.BufferData(gl.ARRAY_BUFFER, len(data), unsafe.Pointer(&data[0]), gl.STATIC_DRAW)
}
func (r *Renderer_GL) SetModelVertexData(bufferIndex uint32, values []byte) {
	gl.BindBuffer(gl.ARRAY_BUFFER, r.modelVertexBuffer[bufferIndex])
	gl.BufferData(gl.ARRAY_BUFFER, len(values), unsafe.Pointer(&values[0]), gl.STATIC_DRAW)
}
func (r *Renderer_GL) SetModelIndexData(bufferIndex uint32, values ...uint32) {
	data := new(bytes.Buffer)
	binary.Write(data, binary.LittleEndian, values)

	gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, r.modelIndexBuffer[bufferIndex])
	gl.BufferData(gl.ELEMENT_ARRAY_BUFFER, len(values)*4, unsafe.Pointer(&data.Bytes()[0]), gl.STATIC_DRAW)
}

func (r *Renderer_GL) RenderQuad() {
	gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4)
	sys_nDrawcall++
}

// Compute the index order for count quads using degenerate stitching.
// Example: 2 quads → strip uses 10 indices (4 + 2 + 4).
func buildDegenerateStripIndices(count int) []int32 {
	if count <= 0 {
		return nil
	}

	// Each quad has exactly 4 vertices
	// Strip requires: 4 + (count-1)*(4+2)
	total := 4 + (count-1)*6
	idx := make([]int32, 0, total)

	// First quad
	idx = append(idx, 0, 1, 2, 3)

	for q := 1; q < count; q++ {
		base := int32(q * 4)

		// Insert degenerates: repeat last + next first
		lastPrev := int32((q-1)*4 + 3)
		idx = append(idx, lastPrev, base)

		// Add quad
		idx = append(idx, base, base+1, base+2, base+3)
	}

	return idx
}

// Renders all quads (4 vertices each) in ONE triangle strip using degenerates.
// No changes to vertex data layout required.
func (r *Renderer_GL) RenderQuadsStrip(count int) {
	if count <= 0 {
		return
	}

	indices := buildDegenerateStripIndices(count)
	gl.DrawElements(gl.TRIANGLE_STRIP, int32(len(indices)), gl.UNSIGNED_INT,
		gl.Ptr(indices))
	sys_nDrawcall++
}

func (r *Renderer_GL) RenderElements(mode PrimitiveMode, count, offset int) {
	gl.DrawElementsWithOffset(r.MapPrimitiveMode(mode), int32(count), gl.UNSIGNED_INT, uintptr(offset))
	sys_nDrawcall++
}
func (r *Renderer_GL) RenderShadowMapElements(mode PrimitiveMode, count, offset int) {
	r.RenderElements(mode, count, offset)
}

func (r *Renderer_GL) RenderCubeMap(envTex Texture, cubeTex Texture) {
	envTexture := envTex.(*Texture_GL)
	cubeTexture := cubeTex.(*Texture_GL)
	textureSize := cubeTexture.width
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_env)
	gl.Viewport(0, 0, textureSize, textureSize)
	gl.UseProgram(r.panoramaToCubeMapShader.program)
	loc := r.panoramaToCubeMapShader.a["VertCoord"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 0, 0)
	data := f32.Bytes(binary.LittleEndian, -1, -1, 1, -1, -1, 1, 1, 1)
	gl.BindBuffer(gl.ARRAY_BUFFER, r.vertexBuffer)
	gl.BufferData(gl.ARRAY_BUFFER, len(data), unsafe.Pointer(&data[0]), gl.STATIC_DRAW)
	loc, unit := r.panoramaToCubeMapShader.u["panorama"], r.panoramaToCubeMapShader.t["panorama"]
	gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
	gl.BindTexture(gl.TEXTURE_2D, envTexture.handle)
	gl.Uniform1i(loc, int32(unit))
	for i := 0; i < 6; i++ {
		gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, uint32(gl.TEXTURE_CUBE_MAP_POSITIVE_X+i), cubeTexture.handle, 0)

		gl.Clear(gl.COLOR_BUFFER_BIT)
		loc := r.panoramaToCubeMapShader.u["currentFace"]
		gl.Uniform1i(loc, int32(i))

		gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4)
		sys_nDrawcall++
	}
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo)
	gl.BindTexture(gl.TEXTURE_CUBE_MAP, cubeTexture.handle)
	gl.GenerateMipmap(gl.TEXTURE_CUBE_MAP)
}
func (r *Renderer_GL) RenderFilteredCubeMap(distribution int32, cubeTex Texture, filteredTex Texture, mipmapLevel, sampleCount int32, roughness float32) {
	cubeTexture := cubeTex.(*Texture_GL)
	filteredTexture := filteredTex.(*Texture_GL)
	textureSize := filteredTexture.width
	currentTextureSize := textureSize >> mipmapLevel
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_env)
	gl.Viewport(0, 0, currentTextureSize, currentTextureSize)
	gl.UseProgram(r.cubemapFilteringShader.program)
	loc := r.cubemapFilteringShader.a["VertCoord"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 0, 0)
	data := f32.Bytes(binary.LittleEndian, -1, -1, 1, -1, -1, 1, 1, 1)
	gl.BindBuffer(gl.ARRAY_BUFFER, r.vertexBuffer)
	gl.BufferData(gl.ARRAY_BUFFER, len(data), unsafe.Pointer(&data[0]), gl.STATIC_DRAW)
	loc, unit := r.cubemapFilteringShader.u["cubeMap"], r.cubemapFilteringShader.t["cubeMap"]
	gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
	gl.BindTexture(gl.TEXTURE_CUBE_MAP, cubeTexture.handle)
	gl.Uniform1i(loc, int32(unit))
	loc = r.cubemapFilteringShader.u["sampleCount"]
	gl.Uniform1i(loc, sampleCount)
	loc = r.cubemapFilteringShader.u["distribution"]
	gl.Uniform1i(loc, distribution)
	loc = r.cubemapFilteringShader.u["width"]
	gl.Uniform1i(loc, textureSize)
	loc = r.cubemapFilteringShader.u["roughness"]
	gl.Uniform1f(loc, roughness)
	loc = r.cubemapFilteringShader.u["intensityScale"]
	gl.Uniform1f(loc, 1)
	loc = r.cubemapFilteringShader.u["isLUT"]
	gl.Uniform1i(loc, 0)
	for i := 0; i < 6; i++ {
		gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, uint32(gl.TEXTURE_CUBE_MAP_POSITIVE_X+i), filteredTexture.handle, mipmapLevel)

		gl.Clear(gl.COLOR_BUFFER_BIT)
		loc := r.cubemapFilteringShader.u["currentFace"]
		gl.Uniform1i(loc, int32(i))

		gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4)
		sys_nDrawcall++
	}
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo)
}
func (r *Renderer_GL) RenderLUT(distribution int32, cubeTex Texture, lutTex Texture, sampleCount int32) {
	cubeTexture := cubeTex.(*Texture_GL)
	lutTexture := lutTex.(*Texture_GL)
	textureSize := lutTexture.width
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo_env)
	gl.Viewport(0, 0, textureSize, textureSize)
	gl.UseProgram(r.cubemapFilteringShader.program)
	loc := r.cubemapFilteringShader.a["VertCoord"]
	gl.EnableVertexAttribArray(uint32(loc))
	gl.VertexAttribPointerWithOffset(uint32(loc), 2, gl.FLOAT, false, 0, 0)
	data := f32.Bytes(binary.LittleEndian, -1, -1, 1, -1, -1, 1, 1, 1)
	gl.BindBuffer(gl.ARRAY_BUFFER, r.vertexBuffer)
	gl.BufferData(gl.ARRAY_BUFFER, len(data), unsafe.Pointer(&data[0]), gl.STATIC_DRAW)
	loc, unit := r.cubemapFilteringShader.u["cubeMap"], r.cubemapFilteringShader.t["cubeMap"]
	gl.ActiveTexture((uint32(gl.TEXTURE0 + unit)))
	gl.BindTexture(gl.TEXTURE_CUBE_MAP, cubeTexture.handle)
	gl.Uniform1i(loc, int32(unit))
	loc = r.cubemapFilteringShader.u["sampleCount"]
	gl.Uniform1i(loc, sampleCount)
	loc = r.cubemapFilteringShader.u["distribution"]
	gl.Uniform1i(loc, distribution)
	loc = r.cubemapFilteringShader.u["width"]
	gl.Uniform1i(loc, textureSize)
	loc = r.cubemapFilteringShader.u["roughness"]
	gl.Uniform1f(loc, 0)
	loc = r.cubemapFilteringShader.u["intensityScale"]
	gl.Uniform1f(loc, 1)
	loc = r.cubemapFilteringShader.u["currentFace"]
	gl.Uniform1i(loc, 0)
	loc = r.cubemapFilteringShader.u["isLUT"]
	gl.Uniform1i(loc, 1)

	gl.BindTexture(gl.TEXTURE_2D, lutTexture.handle)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, lutTexture.width, lutTexture.height, 0, gl.RGBA, gl.FLOAT, nil)

	gl.FramebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, lutTexture.handle, 0)
	gl.Clear(gl.COLOR_BUFFER_BIT)
	gl.DrawArrays(gl.TRIANGLE_STRIP, 0, 4)
	sys_nDrawcall++
	gl.BindFramebuffer(gl.FRAMEBUFFER, r.fbo)
}

// ------------------------------------------------------------------
// File I/O / SFF loader helpers and structures

func (r *Renderer_GL) PerspectiveProjectionMatrix(angle, aspect, near, far float32) mgl.Mat4 {
	return mgl.Perspective(angle, aspect, near, far)
}

func (r *Renderer_GL) OrthographicProjectionMatrix(left, right, bottom, top, near, far float32) mgl.Mat4 {
	ret := mgl.Ortho(left, right, bottom, top, near, far)
	return ret
}

func (r *Renderer_GL) NewWorkerThread() bool {
	return false
}

func (r *Renderer_GL) SetVSync() {
}

var sys_cfg_Config_PaletteMax = 256
var sys_cfg_Video_RGBSpriteBilinearFilter = true

// OpenFile opens a regular file or a file within a zip archive.
// For zip files, it reads the entire entry into memory to ensure full io.Seeker compatibility.
// It returns an io.ReadSeekCloser that must be closed by the caller.
func OpenFile(filename string) (io.ReadSeekCloser, error) {
	filename = filepath.ToSlash(filename)
	// Not a zip path, open as a normal file
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	return f, nil // *os.File implements io.ReadSeekCloser
}

// (SffHeader defined in the types section near the top)

func (sh *SffHeader) Read(r io.Reader, lofs *uint32, tofs *uint32) error {
	buf := make([]byte, 12)
	n, err := r.Read(buf)
	if err != nil {
		return err
	}
	if string(buf[:n]) != "ElecbyteSpr\x00" {
		return Error("Unrecognized SFF file, invalid header")
	}
	read := func(x interface{}) error {
		return binary.Read(r, binary.LittleEndian, x)
	}
	if err := read(&sh.Ver3); err != nil {
		return err
	}
	if err := read(&sh.Ver2); err != nil {
		return err
	}
	if err := read(&sh.Ver1); err != nil {
		return err
	}
	if err := read(&sh.Ver0); err != nil {
		return err
	}
	var dummy uint32
	if err := read(&dummy); err != nil {
		return err
	}
	switch sh.Ver0 {
	case 1:
		sh.FirstPaletteHeaderOffset, sh.NumberOfPalettes = 0, 0
		if err := read(&sh.NumberOfSprites); err != nil {
			return err
		}
		if err := read(&sh.FirstSpriteHeaderOffset); err != nil {
			return err
		}
		if err := read(&dummy); err != nil {
			return err
		}
	case 2:
		for i := 0; i < 4; i++ {
			if err := read(&dummy); err != nil {
				return err
			}
		}
		if err := read(&sh.FirstSpriteHeaderOffset); err != nil {
			return err
		}
		if err := read(&sh.NumberOfSprites); err != nil {
			return err
		}
		if err := read(&sh.FirstPaletteHeaderOffset); err != nil {
			return err
		}
		if err := read(&sh.NumberOfPalettes); err != nil {
			return err
		}
		if err := read(lofs); err != nil {
			return err
		}
		if err := read(&dummy); err != nil {
			return err
		}
		if err := read(tofs); err != nil {
			return err
		}
	default:
		return Error("Unrecognized SFF version")
	}
	return nil
}

// (SFF cache / palette initializer functions are declared in the types section above)

func (s *Sprite) isBlank() bool {
	return s.Tex == nil || s.Size[0] == 0 || s.Size[1] == 0
}

func newSprite() *Sprite {
	// Initialize palidx to -1 to indicate no palette assigned and UV to zeroed
	return &Sprite{palidx: -1, UV: [4]float32{0, 0, 0, 0}}
}

func (s *Sprite) readHeader(r io.Reader, ofs, size *uint32, link *uint16) error {
	read := func(x interface{}) error {
		return binary.Read(r, binary.LittleEndian, x)
	}
	if err := read(ofs); err != nil {
		return err
	}
	if err := read(size); err != nil {
		return err
	}
	if err := read(s.Offset[:]); err != nil {
		return err
	}
	if err := read(&s.Group); err != nil {
		return err
	}
	if err := read(&s.Number); err != nil {
		return err
	}
	if err := read(link); err != nil {
		return err
	}
	return nil
}

func (s *Sprite) readPcxHeader(r io.ReadSeeker, offset int64) error {
	if _, err := r.Seek(offset, io.SeekStart); err != nil {
		return fmt.Errorf("readPcxHeader seek error: %w", err)
	}
	read := func(rd io.Reader, x interface{}) error { // Helper takes io.Reader
		return binary.Read(rd, binary.LittleEndian, x)
	}
	var dummy uint16
	if err := read(r, &dummy); err != nil {
		return err
	}
	var encoding, bpp byte
	if err := read(r, &encoding); err != nil {
		return err
	}
	if err := read(r, &bpp); err != nil {
		return err
	}
	if bpp != 8 {
		return Error(fmt.Sprintf("Invalid PCX color depth: expected 8-bit, got %v", bpp))
	}
	var rect [4]uint16
	if err := read(r, rect[:]); err != nil {
		return err
	}
	if _, err := r.Seek(offset+66, io.SeekStart); err != nil { // Use r.Seek
		return fmt.Errorf("readPcxHeader seek to bpl error: %w", err)
	}
	var bpl uint16
	if err := read(r, &bpl); err != nil {
		return err
	}
	s.Size[0] = rect[2] - rect[0] + 1
	s.Size[1] = rect[3] - rect[1] + 1
	if encoding == 1 {
		s.rle = int(bpl)
	} else {
		s.rle = 0
	}
	return nil
}

func (s *Sprite) RlePcxDecode(rle []byte) (p []byte) {
	if len(rle) == 0 || s.rle <= 0 {
		return rle
	}
	p = make([]byte, int(s.Size[0])*int(s.Size[1]))
	i, j, k, w := 0, 0, 0, int(s.Size[0])
	for j < len(p) {
		n, d := 1, rle[i]
		if i < len(rle)-1 {
			i++
		}
		if d >= 0xc0 {
			n = int(d & 0x3f)
			d = rle[i]
			if i < len(rle)-1 {
				i++
			}
		}
		for ; n > 0; n-- {
			if k < w && j < len(p) {
				p[j] = d
				j++
			}
			k++
			if k == s.rle {
				k = 0
				n = 1
			}
		}
	}
	s.rle = 0
	return
}
func (pl *PaletteList) SetSource(i int, p []uint32) {
	if i < len(pl.paletteMap) {
		pl.paletteMap[i] = i
	} else {
		for i > len(pl.paletteMap) {
			pl.paletteMap = append(pl.paletteMap, len(pl.paletteMap))
		}
		pl.paletteMap = append(pl.paletteMap, i)
	}
	if i < len(pl.palettes) {
		pl.palettes[i] = p
	} else {
		for i > len(pl.palettes) {
			pl.palettes = append(pl.palettes, nil)
		}
		pl.palettes = append(pl.palettes, p)
		pl.PalTex = append(pl.PalTex, nil)
	}
}
func (pl *PaletteList) NewPal() (i int, p []uint32) {
	i, p = len(pl.palettes), make([]uint32, 256)
	pl.SetSource(i, p)
	return
}
func (s *Sprite) SetPxl(px []byte) {
	if len(px) == 0 {
		return
	}
	if int64(len(px)) != int64(s.Size[0])*int64(s.Size[1]) {
		return
	}
	sys_mainThreadTask <- func() {
		if s.Sff.Atlas_8 == nil { // No atlas, create individual texture
			s.Tex = gfx.newTexture(int32(s.Size[0]), int32(s.Size[1]), 8, false)
			s.Tex.SetData(px)
		} else { // Use atlas
			ok := false
			s.UV, ok = s.Sff.Atlas_8.AddImage(int32(s.Size[0]), int32(s.Size[1]), px)
			if ok {
				s.Tex = s.Sff.Atlas_8.texture
				// fmt.Printf("Added sprite %vx%v to atlas at UV %v,%v - %v,%v.\n", s.Size[0], s.Size[1], s.UV[0], s.UV[1], s.UV[2], s.UV[3])
			} else {
				s.Tex = nil
				fmt.Printf("Warning: Sprite atlas full. Skipping sprite %vx%v.\n", s.Size[0], s.Size[1])
			}
		}
	}
}

func (s *Sprite) SetRaw(data []byte, sprWidth int32, sprHeight int32, sprDepth int32) {
	sys_mainThreadTask <- func() {
		s.Tex = gfx.newTexture(sprWidth, sprHeight, sprDepth, sys_cfg_Video_RGBSpriteBilinearFilter)
		s.Tex.SetData(data)
	}
}
func (s *Sprite) read(f io.ReadSeeker, sh *SffHeader, offset int64, datasize uint32,
	nextSubheader uint32, prev *Sprite, pl *PaletteList, c00 bool) error {
	if int64(nextSubheader) > offset {
		// Ignore datasize except last
		datasize = nextSubheader - uint32(offset)
	}
	read := func(x interface{}) error {
		return binary.Read(f, binary.LittleEndian, x)
	}
	var ps byte
	if err := read(&ps); err != nil {
		return err
	}
	paletteSame := ps != 0 && prev != nil
	if err := s.readPcxHeader(f, offset); err != nil {
		return err
	}
	f.Seek(offset+128, 0)
	var palSize uint32
	if c00 || paletteSame {
		palSize = 0
	} else {
		palSize = 768
	}
	if datasize < 128+palSize {
		datasize = 128 + palSize
	}
	px := make([]byte, datasize-(128+palSize))
	if err := read(px); err != nil {
		return err
	}
	if paletteSame {
		if prev != nil {
			s.palidx = prev.palidx
		}
		if s.palidx < 0 {
			s.palidx, _ = pl.NewPal()
		}
	} else {
		var pal []uint32
		s.palidx, pal = pl.NewPal()
		if c00 {
			f.Seek(offset+int64(datasize)-768, 0)
		}
		var rgb [3]byte
		for i := range pal {
			if err := read(rgb[:]); err != nil {
				return err
			}
			var alpha byte = 255
			if i == 0 {
				alpha = 0
			}
			pal[i] = uint32(alpha)<<24 | uint32(rgb[2])<<16 | uint32(rgb[1])<<8 | uint32(rgb[0])
		}
	}
	s.SetPxl(s.RlePcxDecode(px))
	return nil
}

func (s *Sprite) readHeaderV2(r io.Reader, ofs *uint32, size *uint32,
	lofs uint32, tofs uint32, link *uint16) error {
	read := func(x interface{}) error {
		return binary.Read(r, binary.LittleEndian, x)
	}
	if err := read(&s.Group); err != nil {
		return err
	}
	if err := read(&s.Number); err != nil {
		return err
	}
	if err := read(s.Size[:]); err != nil {
		return err
	}
	if err := read(s.Offset[:]); err != nil {
		return err
	}
	if err := read(link); err != nil {
		return err
	}
	var format byte
	if err := read(&format); err != nil {
		return err
	}
	s.rle = -int(format)
	if err := read(&s.coldepth); err != nil {
		return err
	}
	if err := read(ofs); err != nil {
		return err
	}
	if err := read(size); err != nil {
		return err
	}
	var tmp uint16
	if err := read(&tmp); err != nil {
		return err
	}
	s.palidx = int(tmp)
	if err := read(&tmp); err != nil {
		return err
	}
	if tmp&1 == 0 {
		*ofs += lofs
	} else {
		*ofs += tofs
	}
	return nil
}

func (s *Sprite) Rle8Decode(rle []byte) (p []byte) {
	if len(rle) == 0 {
		return rle
	}
	p = make([]byte, int(s.Size[0])*int(s.Size[1]))
	i, j := 0, 0
	for j < len(p) {
		n, d := 1, rle[i]
		if i < len(rle)-1 {
			i++
		}
		if d&0xc0 == 0x40 {
			n = int(d & 0x3f)
			d = rle[i]
			if i < len(rle)-1 {
				i++
			}
		}
		for ; n > 0; n-- {
			if j < len(p) {
				p[j] = d
				j++
			}
		}
	}
	return
}

func (s *Sprite) Rle5Decode(rle []byte) (p []byte) {
	if len(rle) == 0 {
		return rle
	}
	p = make([]byte, int(s.Size[0])*int(s.Size[1]))
	i, j := 0, 0
	for j < len(p) {
		rl := int(rle[i])
		if i < len(rle)-1 {
			i++
		}
		dl := int(rle[i] & 0x7f)
		c := byte(0)
		if rle[i]>>7 != 0 {
			if i < len(rle)-1 {
				i++
			}
			c = rle[i]
		}
		if i < len(rle)-1 {
			i++
		}
		for {
			if j < len(p) {
				p[j] = c
				j++
			}
			rl--
			if rl < 0 {
				dl--
				if dl < 0 {
					break
				}
				c = rle[i] & 0x1f
				rl = int(rle[i] >> 5)
				if i < len(rle)-1 {
					i++
				}
			}
		}
	}
	return
}

func (s *Sprite) Lz5Decode(rle []byte) (p []byte) {
	if len(rle) == 0 {
		return rle
	}
	p = make([]byte, int(s.Size[0])*int(s.Size[1]))
	i, j, n := 0, 0, 0
	ct, cts, rb, rbc := rle[i], uint(0), byte(0), uint(0)
	if i < len(rle)-1 {
		i++
	}
	for j < len(p) {
		d := int(rle[i])
		if i < len(rle)-1 {
			i++
		}
		if ct&byte(1<<cts) != 0 {
			if d&0x3f == 0 {
				d = (d<<2 | int(rle[i])) + 1
				if i < len(rle)-1 {
					i++
				}
				n = int(rle[i]) + 2
				if i < len(rle)-1 {
					i++
				}
			} else {
				rb |= byte(d & 0xc0 >> rbc)
				rbc += 2
				n = int(d & 0x3f)
				if rbc < 8 {
					d = int(rle[i]) + 1
					if i < len(rle)-1 {
						i++
					}
				} else {
					d = int(rb) + 1
					rb, rbc = 0, 0
				}
			}
			for {
				if j < len(p) {
					p[j] = p[j-d]
					j++
				}
				n--
				if n < 0 {
					break
				}
			}
		} else {
			if d&0xe0 == 0 {
				n = int(rle[i]) + 8
				if i < len(rle)-1 {
					i++
				}
			} else {
				n = d >> 5
				d &= 0x1f
			}
			for ; n > 0; n-- {
				if j < len(p) {
					p[j] = byte(d)
					j++
				}
			}
		}
		cts++
		if cts >= 8 {
			ct, cts = rle[i], 0
			if i < len(rle)-1 {
				i++
			}
		}
	}
	return
}

func (s *Sprite) readV2(f io.ReadSeeker, offset int64, datasize uint32) error {
	var px []byte
	var isRaw bool = false

	if s.rle > 0 {
		return nil

	} else if s.rle == 0 {
		f.Seek(offset, 0)
		px = make([]uint8, datasize)
		binary.Read(f, binary.LittleEndian, px)

		switch s.coldepth {
		case 8:
			// Do nothing, px is already in the expected format
		case 24, 32:
			isRaw = true
			s.SetRaw(px, int32(s.Size[0]), int32(s.Size[1]), int32(s.coldepth))
		default:
			return Error("Unknown color depth")
		}

	} else {
		f.Seek(offset+4, 0)
		format := -s.rle

		var rgba *image.RGBA
		var rect image.Rectangle

		if 2 <= format && format <= 4 {
			if datasize < 4 {
				datasize = 4
			}
			px = make([]byte, datasize-4)
			if err := binary.Read(f, binary.LittleEndian, px); err != nil {
				panic(err)
				//return err
			}
		}

		switch format {
		case 2:
			px = s.Rle8Decode(px)
		case 3:
			px = s.Rle5Decode(px)
		case 4:
			px = s.Lz5Decode(px)
		case 10:
			img, err := png.Decode(f)
			if err != nil {
				return err
			}
			pi, ok := img.(*image.Paletted)
			if ok {
				px = pi.Pix
			}
		case 11, 12:
			var ok bool = false
			isRaw = true

			// Decode PNG image to RGBA
			img, err := png.Decode(f)
			if err != nil {
				return err
			}

			rect = img.Bounds()
			rgba, ok = img.(*image.RGBA)

			if !ok {
				rgba = image.NewRGBA(rect)
				draw.Draw(rgba, rect, img, rect.Min, draw.Src)
			}
			s.SetRaw(rgba.Pix, int32(rect.Max.X-rect.Min.X), int32(rect.Max.Y-rect.Min.Y), 32)
		default:
			return Error("Unknown format")
		}
	}

	if !isRaw {
		s.SetPxl(px)
	}
	return nil
}
func (pl *PaletteList) Get(i int) []uint32 {
	return pl.palettes[pl.paletteMap[i]]
}
func (s *Sprite) shareCopy(src *Sprite) {
	s.Pal = src.Pal
	s.Tex = src.Tex
	s.Size = src.Size
	if s.palidx < 0 {
		s.palidx = src.palidx
	}
	s.coldepth = src.coldepth
	//s.paltemp = src.paltemp
	//s.PalTex = src.PalTex
}
func convertPaletteToBytes(pal []uint32) []byte {
	data := make([]byte, len(pal)*4)
	for i, color := range pal {
		data[i*4] = byte(color)         // R
		data[i*4+1] = byte(color >> 8)  // G
		data[i*4+2] = byte(color >> 16) // B
		data[i*4+3] = byte(color >> 24) // A
	}
	return data
}
func loadSff(filename string, char bool) (*Sff, error) {
	// If this SFF is already in the cache, just return a copy
	if cached, ok := SffCache[filename]; ok {
		cached.refCount++
		s := cached.sffData
		return &s, nil
	}
	s := newSff()
	s.filename = filename
	f, err := OpenFile(filename)
	if err != nil {
		return nil, err
	}
	defer func() { chk(f.Close()) }()
	var lofs, tofs uint32
	if err := s.header.Read(f, &lofs, &tofs); err != nil {
		return nil, err
	}
	read := func(x interface{}) error {
		return binary.Read(f, binary.LittleEndian, x)
	}
	fmt.Printf("SFF Version: %d.%d.%d.%d\n", s.header.Ver0, s.header.Ver1, s.header.Ver2, s.header.Ver3)
	if s.header.Ver0 != 1 {
		uniquePals := make(map[[2]uint16]int)
		for i := 0; i < int(s.header.NumberOfPalettes); i++ {
			f.Seek(int64(s.header.FirstPaletteHeaderOffset)+int64(i*16), 0)
			var gn_ [3]uint16
			if err := read(gn_[:]); err != nil {
				return nil, err
			}
			var link uint16
			if err := read(&link); err != nil {
				return nil, err
			}
			var ofs, siz uint32
			if err := read(&ofs); err != nil {
				return nil, err
			}
			if err := read(&siz); err != nil {
				return nil, err
			}
			var pal []uint32
			var idx int
			if old, ok := uniquePals[[...]uint16{gn_[0], gn_[1]}]; ok {
				idx = old
				pal = s.palList.Get(old)
				fmt.Printf("%v duplicated palette: %v,%v (%v/%v)\n", filename, gn_[0], gn_[1], i+1, s.header.NumberOfPalettes)
			} else if siz == 0 {
				idx = int(link)
				pal = s.palList.Get(idx)
			} else {
				f.Seek(int64(lofs+ofs), 0)
				pal = make([]uint32, 256)
				var rgba [4]byte
				for i := 0; i < int(siz)/4 && i < len(pal); i++ {
					if err := read(rgba[:]); err != nil {
						return nil, err
					}
					if s.header.Ver2 == 0 {
						if i == 0 {
							rgba[3] = 0
						} else {
							rgba[3] = 255
						}
					}
					pal[i] = uint32(rgba[3])<<24 | uint32(rgba[2])<<16 | uint32(rgba[1])<<8 | uint32(rgba[0])
				}
				idx = i
			}
			uniquePals[[...]uint16{gn_[0], gn_[1]}] = idx
			s.palList.SetSource(i, pal)
			s.palList.PalTable[[...]uint16{gn_[0], gn_[1]}] = idx
			s.palList.numcols[[...]uint16{gn_[0], gn_[1]}] = int(gn_[2])
			if i <= sys_cfg_Config_PaletteMax &&
				s.palList.PalTable[[...]uint16{1, uint16(i + 1)}] == s.palList.PalTable[[...]uint16{gn_[0], gn_[1]}] &&
				gn_[0] != 1 && gn_[1] != uint16(i+1) {
				s.palList.PalTable[[...]uint16{1, uint16(i + 1)}] = -1
			}
			if i <= sys_cfg_Config_PaletteMax && i+1 == int(s.header.NumberOfPalettes) {
				for j := i + 1; j < sys_cfg_Config_PaletteMax; j++ {
					delete(s.palList.PalTable, [...]uint16{1, uint16(j + 1)}) // Remove extra palette
				}
			}
		}
	}
	spriteList := make([]*Sprite, int(s.header.NumberOfSprites))
	var prev *Sprite
	shofs := int64(s.header.FirstSpriteHeaderOffset)
	if !char {
		s.Atlas_8 = CreateTextureAtlas(512, 256, 8, false)
		s.Atlas_8.resize = true
	}
	for i := 0; i < len(spriteList); i++ {
		f.Seek(shofs, 0)
		spriteList[i] = newSprite()
		spriteList[i].Sff = s
		var xofs, size uint32
		var indexOfPrevious uint16
		switch s.header.Ver0 {
		case 1:
			if err := spriteList[i].readHeader(f, &xofs, &size,
				&indexOfPrevious); err != nil {
				return nil, err
			}
		case 2:
			if err := spriteList[i].readHeaderV2(f, &xofs, &size,
				lofs, tofs, &indexOfPrevious); err != nil {
				return nil, err
			}
		}
		if size == 0 {
			if int(indexOfPrevious) < i {
				dst, src := spriteList[i], spriteList[int(indexOfPrevious)]
				sys_mainThreadTask <- func() {
					dst.shareCopy(src)
				}
			} else {
				spriteList[i].palidx = 0 // index out of range
			}
		} else {
			switch s.header.Ver0 {
			case 1:
				if err := spriteList[i].read(f, &s.header, shofs+32, size,
					xofs, prev, &s.palList,
					char && (prev == nil || spriteList[i].Group == 0 &&
						spriteList[i].Number == 0)); err != nil {
					return nil, err
				}
			case 2:
				if err := spriteList[i].readV2(f, int64(xofs), size); err != nil {
					return nil, err
				}
			}
			prev = spriteList[i]
		}
		if s.sprites[[...]uint16{spriteList[i].Group, spriteList[i].Number}] ==
			nil {
			s.sprites[[...]uint16{spriteList[i].Group, spriteList[i].Number}] =
				spriteList[i]
		}
		if s.header.Ver0 == 1 {
			shofs = int64(xofs)
		} else {
			shofs += 28
		}
	}
	SffCache[filename] = &SffCacheEntry{*s, 1}
	runtime.SetFinalizer(s, func(s *Sff) {
		if cached, ok := SffCache[filename]; ok {
			cached.refCount--
			if cached.refCount == 0 {
				delete(SffCache, filename)
			}
		}
	})
	if s.Atlas_8 != nil {
		var defpal []uint32
		if len(s.palList.palettes) > 0 {
			defpal = s.palList.Get(0)
		}
		// Schedule atlas saving on the main thread so we capture the uploaded
		// atlas contents after any queued AddImage() / SetData operations finish.
		log.Printf("loadSff: scheduling delayed save of atlas_8.png (palette present=%t length=%d)", defpal != nil, len(defpal))
		sys_mainThreadTask <- func() {
			if s.Atlas_8 == nil || s.Atlas_8.texture == nil {
				log.Printf("loadSff (delayed): Atlas_8 or its texture is nil - cannot save atlas_8.png")
				return
			}
			log.Printf("loadSff (delayed): performing SavePNG on atlas_8.png (texture valid=%t)", s.Atlas_8.texture.IsValid())
			if err := s.Atlas_8.texture.SavePNG("atlas_8.png", defpal); err != nil {
				log.Printf("loadSff (delayed): SavePNG returned error: %v", err)
			}
		}
	}
	return s, nil
}

// ------------------------------------------------------------------
// Sample / example main (entry point)

// (types PaletteList, Sff and Palette were moved up to the "Types" section near the top)

// Or better yet, let's add proper diagnostics to your existing code:
func main() {
	window := initGLFW()
	defer glfw.Terminate()
	initOpenGL()

	var max_texture_size int32
	gl.GetIntegerv(gl.MAX_TEXTURE_SIZE, &max_texture_size)
	fmt.Printf("Maximum texture size: %d x %d\n", max_texture_size, max_texture_size)

	sff, err := loadSff("kfm.sff", true)
	if err != nil {
		panic(err)
	}

	key := [...]uint16{uint16(0), uint16(0)}
	src, ok := sff.sprites[key]
	if !ok || src == nil {
		panic("Sprite Error")
	}

	gl.ClearColor(0.1, 0.1, 0.1, 1.0)
	gfx.Init()

	// Process any pending GPU tasks to ensure textures are uploaded
	sys_runMainThreadTask()

	fnt, err := loadFntV2("f-6x9.def", 9)
	if err != nil {
		panic(err)
	}

	if src.coldepth <= 8 && src.PalTex == nil {
		src.PalTex = src.CachePalette(src.GetPal(&sff.palList))
	}

	// Re-check texture status after processing tasks
	if src.Tex != nil {
		fmt.Printf("Texture after task processing - valid: %t, Size: %dx%d\n",
			src.Tex.IsValid(), src.Tex.GetWidth(), src.Tex.GetHeight())
	}
	if src.PalTex != nil {
		fmt.Printf("Palette after task processing - valid: %t, Size: %dx%d\n",
			src.PalTex.IsValid(), src.PalTex.GetWidth(), src.PalTex.GetHeight())
	} else {
		fmt.Printf("Palette is NIL\n")
	}

	glfw.SwapInterval(1)

	for !window.ShouldClose() {
		glfw.PollEvents()
		gfx.BeginFrame(true) // true to clear the frame
		updateFPS()
		src.Draw(100, 100, 1, 1, 0, Rotation{}, sys_allPalFX, &sys_scrrect)
		src.Draw(200, 200, 1, 1, 0, Rotation{}, sys_allPalFX, &sys_scrrect)
		src.Draw(300, 300, 1, 1, 0, Rotation{}, sys_allPalFX, &sys_scrrect)
		src.Draw(400, 400, 1, 1, 0, Rotation{}, sys_allPalFX, &sys_scrrect)
		fnt.DrawText("Test DrawText Ikemen Go 100,100", 100, 100, 1, 1, 0, Rotation{}, 0, 0, &sys_scrrect, sys_allPalFX, 1.0)
		fnt.DrawText("Test DrawText Ikemen Go 200,200", 200, 200, 1, 1, 0, Rotation{}, 0, 0, &sys_scrrect, sys_allPalFX, 1.0)
		fnt.DrawText("Test DrawText Ikemen Go 300,300", 300, 300, 1, 1, 0, Rotation{}, 0, 0, &sys_scrrect, sys_allPalFX, 1.0)
		fnt.DrawText("Test DrawText Ikemen Go 400,400", 400, 400, 1, 1, 0, Rotation{}, 0, 0, &sys_scrrect, sys_allPalFX, 1.0)
		fpsText := fmt.Sprintf("FPS: %.1f | Draw Calls: %d", sys_gameFPS, sys_Drawcall)
		fnt.DrawText(fpsText, 450, 450, 1, 1, 0, Rotation{}, 0, 0, &sys_scrrect, sys_allPalFX, 1.0)
		gfx.EndFrame()
		window.SwapBuffers()
		// gfx.Await()

		// Process GPU tasks every frame
		// sys_runMainThreadTask()
	}
	gfx.Close()
}
