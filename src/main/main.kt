import io.reactivex.Observable.interval
import org.joml.Matrix4f
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL.createCapabilities
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.glBindVertexArray
import org.lwjgl.opengl.GL30.glGenVertexArrays
import org.lwjgl.system.MemoryUtil.NULL
import rendering.Mesh
import rendering.Scalar
import symbolic.*
import java.io.File
import java.util.concurrent.TimeUnit

val foo = Variable<Scalar>("foo")
val triangle = SymMesh(CSymIterable(
		listOf(
				CSymFace(
						CSymVector3(foo, ScalarC(0.0f), ScalarC(0.0f)),
						CSymVector3(ScalarC(1.0f), ScalarC(0.0f), ScalarC(0.0f)),
						CSymVector3(ScalarC(0.0f), ScalarC(1.0f), ScalarC(0.0f))))))

fun main(args: Array<String>) {

	val cb = GLFWErrorCallback.createPrint(System.err)

	glfwSetErrorCallback(cb)

	if (!glfwInit()) {
		throw IllegalStateException("Unable to initialize GLFW")
	}

	// GLWF and OpenGL setup

	glfwDefaultWindowHints()
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE)


	val WIDTH = 1024
	val HEIGHT = 768
	val window = glfwCreateWindow(WIDTH, HEIGHT, "Drawy Visualizer", NULL, NULL)

	if (window == NULL) {
		glfwTerminate()
		throw RuntimeException("Failed to create the GLFW window")
	}

	// Create an OpenGL context associated with the window
	glfwMakeContextCurrent(window)

	// Initialize LWJGL with the context
	createCapabilities()

	glfwSwapInterval(1)

	glClearColor(1.0f, 0.0f, 0.0f, 1.0f)

	// Shaders

	val shaderProgram = linkShaders(
			loadAndCompileShader(GL_VERTEX_SHADER, File("shaders/vertex.glsl")),
			loadAndCompileShader(GL_FRAGMENT_SHADER, File("shaders/fragment.glsl")))

	glUseProgram(shaderProgram)

	// Load the finalMeshStream

	val vao = glGenVertexArrays()
	glBindVertexArray(vao)

	val timer = interval(1000 / 60, TimeUnit.MILLISECONDS)

	val vbo = glGenBuffers()

	val finalMeshStream = timer.map {
		triangle.substitute(foo, ScalarC(Math.sin(it.toDouble() / 10.0).toFloat())).eval()
	}

	val view = Matrix4f().identity()

	val ratio = WIDTH.toFloat() / HEIGHT.toFloat()
	val projection = Matrix4f().ortho(-ratio, ratio, -1f, 1f, -1f, 1f)
	val modelviewprojection = projection.mul(view, Matrix4f())

	val uniView = glGetUniformLocation(shaderProgram, "modelview")
	glUniformMatrix4fv(uniView, false, view.get(FloatArray(16)))

	val uniProjection = glGetUniformLocation(shaderProgram, "modelviewproj")
	glUniformMatrix4fv(uniProjection, false, modelviewprojection.get(FloatArray(16)))

	finalMeshStream.blockingForEach({
										mesh ->
										drawFrame(vbo, mesh, shaderProgram, window)

										//!glfwWindowShouldClose(window)

									})

	glfwDestroyWindow(window)

	glfwTerminate()
	cb.free()

}

private fun drawFrame(vbo: Int, mesh: Mesh, shaderProgram: Int, window: Long) {
	glBindBuffer(GL_ARRAY_BUFFER, vbo)
	glBufferData(GL_ARRAY_BUFFER,
				 mesh.vertices
						 .flatMap { listOf(it.x, it.y, it.z) }
						 .toFloatArray(), GL_DYNAMIC_DRAW)

	val posAttrib = glGetAttribLocation(shaderProgram, "position")
	glEnableVertexAttribArray(posAttrib)

	val BYTES_PER_FLOAT = 4
	glVertexAttribPointer(posAttrib, 3, GL_FLOAT, false, 3 * BYTES_PER_FLOAT, 0)

	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
	glDrawArrays(GL_TRIANGLES, 0, 3)
	glfwSwapBuffers(window)
	glfwPollEvents()
}

private fun linkShaders(vertexShader: Int, fragmentShader: Int): Int {
	val shaderProgram = glCreateProgram()
	glAttachShader(shaderProgram, vertexShader)
	glAttachShader(shaderProgram, fragmentShader)
	glLinkProgram(shaderProgram)

	val status = glGetProgrami(shaderProgram, GL_LINK_STATUS)
	if (status != GL_TRUE) {
		throw RuntimeException(glGetProgramInfoLog(shaderProgram))
	}

	return shaderProgram
}

private fun loadAndCompileShader(shaderType: Int, file: File): Int {
	val vertexShader = glCreateShader(shaderType)
	glShaderSource(vertexShader, file.readText())
	glCompileShader(vertexShader)

	val status = glGetShaderi(vertexShader, GL_COMPILE_STATUS)
	if (status != GL_TRUE) {
		throw RuntimeException(glGetShaderInfoLog(vertexShader))
	}

	return vertexShader
}
