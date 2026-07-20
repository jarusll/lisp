(ql:quickload 'cffi)
(ql:quickload 'cffi-libffi)

(cffi:define-foreign-library libraylib
    (:unix (:default "/usr/local/lib/libraylib"))
    (t (:default "libraylib")))

; / Vector2, 2 components
; typedef struct Vector2 {
;     float x;                // Vector x component
;     float y;                // Vector y component
; } Vector2;

(cffi:defcstruct (%Vector2 :class vector2-type)
    (x :float)
    (y :float))

(defstruct vector2
    x
    y)

; // Vector3, 3 components
; typedef struct Vector3 {
;     float x;                // Vector x component
;     float y;                // Vector y component
;     float z;                // Vector z component
; } Vector3;

(cffi:defcstruct (%Vector3 :class vector3-type)
    (x :float)
    (y :float)
    (z :float))

(defstruct vector3
    x
    y
    z)

; // Vector4, 4 components
; typedef struct Vector4 {
;     float x;                // Vector x component
;     float y;                // Vector y component
;     float z;                // Vector z component
;     float w;                // Vector w component
; } Vector4;

(cffi:defcstruct (%Vector4 :class vector4-type)
    (x :float)
    (y :float)
    (z :float)
    (w :float))

(defstruct vector4
    x
    y
    z
    w)

; // Quaternion, 4 components (Vector4 alias)
; typedef Vector4 Quaternion;

; // Matrix, 4x4 components, column major, OpenGL style, right-handed
; typedef struct Matrix {
;     float m0, m4, m8, m12;  // Matrix first row (4 components)
;     float m1, m5, m9, m13;  // Matrix second row (4 components)
;     float m2, m6, m10, m14; // Matrix third row (4 components)
;     float m3, m7, m11, m15; // Matrix fourth row (4 components)
; } Matrix;

(cffi:defcstruct (%Matrix :class matrix-type)
    (m0 :float)
    (m4 :float)
    (m8 :float)
    (m12 :float)
    (m1 :float)
    (m5 :float)
    (m9 :float)
    (m13 :float)
    (m2 :float)
    (m6 :float)
    (m10 :float)
    (m14 :float)
    (m3 :float)
    (m7 :float)
    (m11 :float)
    (m15 :float))

(defstruct matrix
    m0
    m4
    m8
    m12
    m1
    m5
    m9
    m13
    m2
    m6
    m10
    m14
    m3
    m7
    m11
    m15)

; // Color, 4 components, R8G8B8A8 (32bit)
; typedef struct Color {
;     unsigned char r;        // Color red value
;     unsigned char g;        // Color green value
;     unsigned char b;        // Color blue value
;     unsigned char a;        // Color alpha value
; } Color;

(cffi:defcstruct (%Color :class color-type)
    (r :unsigned-char)
    (g :unsigned-char)
    (b :unsigned-char)
    (a :unsigned-char))

(defstruct color
    r
    g
    b
    a)

; // Rectangle, 4 components
; typedef struct Rectangle {
;     float x;                // Rectangle top-left corner position x
;     float y;                // Rectangle top-left corner position y
;     float width;            // Rectangle width
;     float height;           // Rectangle height
; } Rectangle;

(cffi:defcstruct (%Rectangle :class rectangle-type)
    (x :float)
    (y :float)
    (width :float)
    (height :float))

(defstruct rectangle
    x
    y
    width
    height)

; // Image, pixel data stored in CPU memory (RAM)
; typedef struct Image {
;     void *data;             // Image raw data
;     int width;              // Image base width
;     int height;             // Image base height
;     int mipmaps;            // Mipmap levels, 1 by default
;     int format;             // Data format (PixelFormat type)
; } Image;

(cffi:defcstruct (%Image :class image-type)
    (data :pointer)
    (width :int)
    (height :int)
    (mipmaps :int)
    (format :int))

(defstruct image
    data
    width
    height
    mipmaps
    format)

; // Texture, tex data stored in GPU memory (VRAM)
; typedef struct Texture {
;     unsigned int id;        // OpenGL texture id
;     int width;              // Texture base width
;     int height;             // Texture base height
;     int mipmaps;            // Mipmap levels, 1 by default
;     int format;             // Data format (PixelFormat type)
; } Texture;

(cffi:defcstruct (%Texture :class texture-type)
    (id :unsigned-int)
    (width :int)
    (height :int)
    (mipmaps :int)
    (format :int))

(defstruct texture
    id
    width
    height
    mipmaps
    format)

; // Texture2D, same as Texture
; typedef Texture Texture2D;

; // TextureCubemap, same as Texture
; typedef Texture TextureCubemap;

; // RenderTexture, fbo for texture rendering
; typedef struct RenderTexture {
;     unsigned int id;        // OpenGL framebuffer object id
;     Texture texture;        // Color buffer attachment texture
;     Texture depth;          // Depth buffer attachment texture
; } RenderTexture;

(cffi:defcstruct (%RenderTexture :class render-texture-type)
    (id :unsigned-int)
    (texture (:struct %Texture))
    (depth (:struct %Texture)))

(defstruct render-texture
    id
    texture
    depth)

; // RenderTexture2D, same as RenderTexture
; typedef RenderTexture RenderTexture2D;

; // NPatchInfo, n-patch layout info
; typedef struct NPatchInfo {
;     Rectangle source;       // Texture source rectangle
;     int left;               // Left border offset
;     int top;                // Top border offset
;     int right;              // Right border offset
;     int bottom;             // Bottom border offset
;     int layout;             // Layout of the n-patch: 3x3, 1x3 or 3x1
; } NPatchInfo;

(cffi:defcstruct (%NPatchInfo :class n-patch-info-type)
    (source (:struct %Rectangle))
    (left :int)
    (top :int)
    (right :int)
    (bottom :int)
    (layout :int))

(defstruct n-patch-info
    source
    left
    top
    right
    bottom
    layout)

; // GlyphInfo, font characters glyphs info
; typedef struct GlyphInfo {
;     int value;              // Character value (Unicode)
;     int offsetX;            // Character offset X when drawing
;     int offsetY;            // Character offset Y when drawing
;     int advanceX;           // Character advance position X
;     Image image;            // Character image data
; } GlyphInfo;

(cffi:defcstruct (%GlyphInfo :class glyph-info-type)
    (value :int)
    (offsetX :int)
    (offsetY :int)
    (advanceX :int)
    (image (:struct %Image)))

(defstruct glyph-info
    value
    offset-x
    offset-y
    advance-x
    image)

; // Font, font texture and GlyphInfo array data
; typedef struct Font {
;     int baseSize;           // Base size (default chars height)
;     int glyphCount;         // Number of glyph characters
;     int glyphPadding;       // Padding around the glyph characters
;     Texture2D texture;      // Texture atlas containing the glyphs
;     Rectangle *recs;        // Rectangles in texture for the glyphs
;     GlyphInfo *glyphs;      // Glyphs info data
; } Font;

(cffi:defcstruct (%Font :class font-type)
    (baseSize :int)
    (glyphCount :int)
    (glyphPadding :int)
    (texture (:struct %Texture))
    (recs :pointer)
    (glyphs :pointer))

(defstruct font
    base-size
    glyph-count
    glyph-padding
    texture
    recs
    glyphs)

; // Camera, defines position/orientation in 3d space
; typedef struct Camera3D {
;     Vector3 position;       // Camera position
;     Vector3 target;         // Camera target it looks-at
;     Vector3 up;             // Camera up vector (rotation over its axis)
;     float fovy;             // Camera field-of-view aperture in Y (degrees) in perspective, used as near plane height in world units in orthographic
;     int projection;         // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
; } Camera3D;

(cffi:defcstruct (%Camera3D :class camera-3d-type)
    (position (:struct %Vector3))
    (target (:struct %Vector3))
    (up (:struct %Vector3))
    (fovy :float)
    (projection :int))

(defstruct camera-3d
    position
    target
    up
    fovy
    projection)

; typedef Camera3D Camera;    // Camera type fallback, defaults to Camera3D

; // Camera2D, defines position/orientation in 2d space
; typedef struct Camera2D {
;     Vector2 offset;         // Camera offset (screen space offset from window origin)
;     Vector2 target;         // Camera target (world space target point that is mapped to screen space offset)
;     float rotation;         // Camera rotation in degrees (pivots around target)
;     float zoom;             // Camera zoom (scaling around target), must not be set to 0, set to 1.0f for no scale
; } Camera2D;

(cffi:defcstruct (%Camera2D :class camera-2d-type)
    (offset (:struct %Vector2))
    (target (:struct %Vector2))
    (rotation :float)
    (zoom :float))

(defstruct camera-2d
    offset
    target
    rotation
    zoom)

; // Mesh, vertex data and vao/vbo
; typedef struct Mesh {
;     int vertexCount;        // Number of vertices stored in arrays
;     int triangleCount;      // Number of triangles stored (indexed or not)

;     // Vertex attributes data
;     float *vertices;        // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
;     float *texcoords;       // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
;     float *texcoords2;      // Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
;     float *normals;         // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
;     float *tangents;        // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
;     unsigned char *colors;  // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
;     unsigned short *indices; // Vertex indices (in case vertex data comes indexed)

;     // Skin data for animation
;     int boneCount;          // Number of bones (MAX: 256 bones)
;     unsigned char *boneIndices; // Vertex bone indices, up to 4 bones influence by vertex (skinning) (shader-location = 6)
;     float *boneWeights;     // Vertex bone weight, up to 4 bones influence by vertex (skinning) (shader-location = 7)

;     // Runtime animation vertex data (CPU skinning)
;     // NOTE: In case of GPU skinning, not used, pointers are NULL
;     float *animVertices;    // Animated vertex positions (after bones transformations)
;     float *animNormals;     // Animated normals (after bones transformations)

;     // OpenGL identifiers
;     unsigned int vaoId;     // OpenGL Vertex Array Object id
;     unsigned int *vboId;    // OpenGL Vertex Buffer Objects id (default vertex data)
; } Mesh;

(cffi:defcstruct (%Mesh :class mesh-type)
    (vertexCount :int)
    (triangleCount :int)
    (vertices :pointer)
    (texcoords :pointer)
    (texcoords2 :pointer)
    (normals :pointer)
    (tangents :pointer)
    (colors :pointer)
    (indices :pointer)
    (boneCount :int)
    (boneIndices :pointer)
    (boneWeights :pointer)
    (animVertices :pointer)
    (animNormals :pointer)
    (vaoId :unsigned-int)
    (vboId :pointer))

(defstruct mesh
    vertex-count
    triangle-count
    vertices
    texcoords
    texcoords2
    normals
    tangents
    colors
    indices
    bone-count
    bone-indices
    bone-weights
    anim-vertices
    anim-normals
    vao-id
    vbo-id)

; // Shader
; typedef struct Shader {
;     unsigned int id;        // Shader program id
;     int *locs;              // Shader locations array (RL_MAX_SHADER_LOCATIONS)
; } Shader;

(cffi:defcstruct (%Shader :class shader-type)
    (id :unsigned-int)
    (locs :pointer))

(defstruct shader
    id
    locs)

; // MaterialMap
; typedef struct MaterialMap {
;     Texture2D texture;      // Material map texture
;     Color color;            // Material map color
;     float value;            // Material map value
; } MaterialMap;

(cffi:defcstruct (%MaterialMap :class material-map-type)
    (texture (:struct %Texture))
    (color (:struct %Color))
    (value :float))

(defstruct material-map
    texture
    color
    value)

; // Material, includes shader and maps
; typedef struct Material {
;     Shader shader;          // Material shader
;     MaterialMap *maps;      // Material maps array (MAX_MATERIAL_MAPS)
;     float params[4];        // Material generic parameters (if required)
; } Material;

(cffi:defcstruct (%Material :class material-type)
    (shader (:struct %Shader))
    (maps :pointer)
    (params (:array :float 4)))

(defstruct material
    shader
    maps
    params)

; // Transform, vertex transformation data
; typedef struct Transform {
;     Vector3 translation;    // Translation
;     Quaternion rotation;    // Rotation
;     Vector3 scale;          // Scale
; } Transform;

(cffi:defcstruct (%Transform :class transform-type)
    (translation (:struct %Vector3))
    (rotation (:struct %Vector4))
    (scale (:struct %Vector3)))

(defstruct transform
    translation
    rotation
    scale)

; // Anim pose, an array of Transform[]
; typedef Transform *ModelAnimPose;

; // Bone, skeletal animation bone
; typedef struct BoneInfo {
;     char name[32];          // Bone name
;     int parent;             // Bone parent
; } BoneInfo;

(cffi:defcstruct (%BoneInfo :class bone-info-type)
    (name (:array :char 32))
    (parent :int))

(defstruct bone-info
    name
    parent)

; // Skeleton, animation bones hierarchy
; typedef struct ModelSkeleton {
;     unsigned int boneCount; // Number of bones
;     BoneInfo *bones;        // Bones information (skeleton)
;     ModelAnimPose bindPose; // Bones base transformation (Transform[])
; } ModelSkeleton;

(cffi:defcstruct (%ModelSkeleton :class model-skeleton-type)
    (boneCount :unsigned-int)
    (bones :pointer)
    (bindPose :pointer))

(defstruct model-skeleton
    bone-count
    bones
    bind-pose)

; // Model, meshes, materials and animation data
; typedef struct Model {
;     Matrix transform;       // Local transform matrix

;     int meshCount;          // Number of meshes
;     int materialCount;      // Number of materials
;     Mesh *meshes;           // Meshes array
;     Material *materials;    // Materials array
;     int *meshMaterial;      // Mesh material number

;     // Animation data
;     ModelSkeleton skeleton; // Skeleton for animation

;     // Runtime animation data (CPU/GPU skinning)
;     ModelAnimPose currentPose; // Current animation pose (Transform[])
;     Matrix *boneMatrices;   // Bones animated transformation matrices
; } Model;

(cffi:defcstruct (%Model :class model-type)
    (transform (:struct %Matrix))
    (meshCount :int)
    (materialCount :int)
    (meshes :pointer)
    (materials :pointer)
    (meshMaterial :pointer)
    (skeleton (:struct %ModelSkeleton))
    (currentPose :pointer)
    (boneMatrices :pointer))

(defstruct model
    transform
    mesh-count
    material-count
    meshes
    materials
    mesh-material
    skeleton
    current-pose
    bone-matrices)

; // ModelAnimation, contains a full animation sequence
; typedef struct ModelAnimation {
;     char name[32];          // Animation name

;     unsigned int boneCount; // Number of bones (per pose)
;     int keyframeCount;      // Number of animation key frames
;     ModelAnimPose *keyframePoses; // Animation sequence keyframe poses [keyframe][pose]
; } ModelAnimation;

(cffi:defcstruct (%ModelAnimation :class model-animation-type)
    (name (:array :char 32))
    (boneCount :unsigned-int)
    (keyframeCount :int)
    (keyframePoses :pointer))

(defstruct model-animation
    name
    bone-count
    keyframe-count
    keyframe-poses)

; // Ray, ray for raycasting
; typedef struct Ray {
;     Vector3 position;       // Ray position (origin)
;     Vector3 direction;      // Ray direction (normalized)
; } Ray;

(cffi:defcstruct (%Ray :class ray-type)
    (position (:struct %Vector3))
    (direction (:struct %Vector3)))

(defstruct ray
    position
    direction)

; // RayCollision, ray hit information
; typedef struct RayCollision {
;     bool hit;               // Did the ray hit something?
;     float distance;         // Distance to the nearest hit
;     Vector3 point;          // Point of the nearest hit
;     Vector3 normal;         // Surface normal of hit
; } RayCollision;

(cffi:defcstruct (%RayCollision :class ray-collision-type)
    (hit :bool)
    (distance :float)
    (point (:struct %Vector3))
    (normal (:struct %Vector3)))

(defstruct ray-collision
    hit
    distance
    point
    normal)

; // BoundingBox
; typedef struct BoundingBox {
;     Vector3 min;            // Minimum vertex box-corner
;     Vector3 max;            // Maximum vertex box-corner
; } BoundingBox;

(cffi:defcstruct (%BoundingBox :class bounding-box-type)
    (min (:struct %Vector3))
    (max (:struct %Vector3)))

(defstruct bounding-box
    min
    max)

; // Wave, audio wave data
; typedef struct Wave {
;     unsigned int frameCount;    // Total number of frames (considering channels)
;     unsigned int sampleRate;    // Frequency (samples per second)
;     unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;     unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
;     void *data;                 // Buffer data pointer
; } Wave;

(cffi:defcstruct (%Wave :class wave-type)
    (frameCount :unsigned-int)
    (sampleRate :unsigned-int)
    (sampleSize :unsigned-int)
    (channels :unsigned-int)
    (data :pointer))

(defstruct wave
    frame-count
    sample-rate
    sample-size
    channels
    data)

; // Opaque structs declaration
; // NOTE: Actual structs are defined internally in raudio module
; typedef struct rAudioBuffer rAudioBuffer;
; typedef struct rAudioProcessor rAudioProcessor;

; // AudioStream, custom audio stream
; typedef struct AudioStream {
;     rAudioBuffer *buffer;       // Pointer to internal data used by the audio system
;     rAudioProcessor *processor; // Pointer to internal data processor, useful for audio effects

;     unsigned int sampleRate;    // Frequency (samples per second)
;     unsigned int sampleSize;    // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
;     unsigned int channels;      // Number of channels (1-mono, 2-stereo, ...)
; } AudioStream;

(cffi:defcstruct (%AudioStream :class audio-stream-type)
    (buffer :pointer)
    (processor :pointer)
    (sampleRate :unsigned-int)
    (sampleSize :unsigned-int)
    (channels :unsigned-int))

(defstruct audio-stream
    buffer
    processor
    sample-rate
    sample-size
    channels)

; // Sound
; typedef struct Sound {
;     AudioStream stream;         // Audio stream
;     unsigned int frameCount;    // Total number of frames (considering channels)
; } Sound;

(cffi:defcstruct (%Sound :class sound-type)
    (stream (:struct %AudioStream))
    (frameCount :unsigned-int))

(defstruct sound
    stream
    frame-count)

; // Music, audio stream, anything longer than ~10 seconds should be streamed
; typedef struct Music {
;     AudioStream stream;         // Audio stream
;     unsigned int frameCount;    // Total number of frames (considering channels)
;     bool looping;               // Music looping enable

;     int ctxType;                // Type of music context (audio filetype)
;     void *ctxData;              // Audio context data, depends on type
; } Music;

(cffi:defcstruct (%Music :class music-type)
    (stream (:struct %AudioStream))
    (frameCount :unsigned-int)
    (looping :bool)
    (ctxType :int)
    (ctxData :pointer))

(defstruct music
    stream
    frame-count
    looping
    ctx-type
    ctx-data)

; // VrDeviceInfo, Head-Mounted-Display device parameters
; typedef struct VrDeviceInfo {
;     int hResolution;                // Horizontal resolution in pixels
;     int vResolution;                // Vertical resolution in pixels
;     float hScreenSize;              // Horizontal size in meters
;     float vScreenSize;              // Vertical size in meters
;     float eyeToScreenDistance;      // Distance between eye and display in meters
;     float lensSeparationDistance;   // Lens separation distance in meters
;     float interpupillaryDistance;   // IPD (distance between pupils) in meters
;     float lensDistortionValues[4];  // Lens distortion constant parameters
;     float chromaAbCorrection[4];    // Chromatic aberration correction parameters
; } VrDeviceInfo;

(cffi:defcstruct (%VrDeviceInfo :class vr-device-info-type)
    (hResolution :int)
    (vResolution :int)
    (hScreenSize :float)
    (vScreenSize :float)
    (eyeToScreenDistance :float)
    (lensSeparationDistance :float)
    (interpupillaryDistance :float)
    (lensDistortionValues (:array :float 4))
    (chromaAbCorrection (:array :float 4)))

(defstruct vr-device-info
    h-resolution
    v-resolution
    h-screen-size
    v-screen-size
    eye-to-screen-distance
    lens-separation-distance
    interpupillary-distance
    lens-distortion-values
    chroma-ab-correction)

; // VrStereoConfig, VR stereo rendering configuration for simulator
; typedef struct VrStereoConfig {
;     Matrix projection[2];           // VR projection matrices (per eye)
;     Matrix viewOffset[2];           // VR view offset matrices (per eye)
;     float leftLensCenter[2];        // VR left lens center
;     float rightLensCenter[2];       // VR right lens center
;     float leftScreenCenter[2];      // VR left screen center
;     float rightScreenCenter[2];     // VR right screen center
;     float scale[2];                 // VR distortion scale
;     float scaleIn[2];               // VR distortion scale in
; } VrStereoConfig;

(cffi:defcstruct (%VrStereoConfig :class vr-stereo-config-type)
    (projection (:array (:struct %Matrix) 2))
    (viewOffset (:array (:struct %Matrix) 2))
    (leftLensCenter (:array :float 2))
    (rightLensCenter (:array :float 2))
    (leftScreenCenter (:array :float 2))
    (rightScreenCenter (:array :float 2))
    (scale (:array :float 2))
    (scaleIn (:array :float 2)))

(defstruct vr-stereo-config
    projection
    view-offset
    left-lens-center
    right-lens-center
    left-screen-center
    right-screen-center
    scale
    scale-in)

; // File path list
; typedef struct FilePathList {
;     unsigned int count;             // Filepaths entries count
;     char **paths;                   // Filepaths entries
; } FilePathList;

(cffi:defcstruct (%FilePathList :class file-path-list-type)
    (count :unsigned-int)
    (paths :pointer))

(defstruct file-path-list
    count
    paths)

; // Automation event
; typedef struct AutomationEvent {
;     unsigned int frame;             // Event frame
;     unsigned int type;              // Event type (AutomationEventType)
;     int params[4];                  // Event parameters (if required)
; } AutomationEvent;

(cffi:defcstruct (%AutomationEvent :class automation-event-type)
    (frame :unsigned-int)
    (type :unsigned-int)
    (params (:array :int 4)))

(defstruct automation-event
    frame
    type
    params)

; // Automation event list
; typedef struct AutomationEventList {
;     unsigned int capacity;          // Events max entries (MAX_AUTOMATION_EVENTS)
;     unsigned int count;             // Events entries count
;     AutomationEvent *events;        // Events entries
; } AutomationEventList;

(cffi:defcstruct (%AutomationEventList :class automation-event-list-type)
    (capacity :unsigned-int)
    (count :unsigned-int)
    (events :pointer))

(defstruct automation-event-list
    capacity
    count
    events)

(defmacro with-members(members obj type &body body)
    (with-gensyms(obj-var)
        `(let ((,obj-var ,obj))
            (symbol-macrolet
                ,(loop for member in members
                    for accessor = (format nil "~a-~a" type member obj-var)
                    collecting `(,member (,(intern accessor (symbol-package type)) ,obj-var)))
                ,@body))))

(defparameter *color* (make-color :r 0 :g 1 :b 2 :a 3))

(with-members(r g b a) *color* color
    (list r g b a))
