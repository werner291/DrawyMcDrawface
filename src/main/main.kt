import io.reactivex.Observable
import io.reactivex.Observable.interval
import io.reactivex.rxkotlin.Observables
import io.reactivex.rxkotlin.toObservable
import io.reactivex.subjects.PublishSubject
import nl.wernerkroneman.DrawyMcDrawface.zAxisAlignedCylinder
import nl.wernerkroneman.SymboliK.Scalar
import nl.wernerkroneman.SymboliK.ScalarC
import nl.wernerkroneman.SymboliK.Symbolic
import nl.wernerkroneman.SymboliK.Variable
import org.joml.Matrix4f
import org.joml.Vector3f
import org.liquidengine.legui.component.*
import org.liquidengine.legui.component.misc.event.slider.SliderChangeValueEvent
import org.liquidengine.legui.event.Event
import org.liquidengine.legui.listener.EventProcessor
import org.liquidengine.legui.listener.ListenerMap
import org.liquidengine.legui.system.context.Context
import org.liquidengine.legui.system.context.DefaultCallbackKeeper
import org.liquidengine.legui.system.processor.SystemEventProcessor
import org.liquidengine.legui.system.renderer.Renderer
import org.liquidengine.legui.system.renderer.RendererProvider
import org.liquidengine.legui.system.renderer.nvg.NvgRenderer
import org.liquidengine.legui.system.renderer.nvg.NvgRendererProvider
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
import java.io.File
import java.util.concurrent.TimeUnit


fun main(args: Array<String>) {

	val cb = initGlfw()

	val WIDTH = 1024
	val HEIGHT = 768

	val window = createWindowWithContext(WIDTH, HEIGHT)
	// Shaders

	val shaderProgram = linkShaders(
			loadAndCompileShader(GL_VERTEX_SHADER, File("shaders/vertex.glsl")),
			loadAndCompileShader(GL_FRAGMENT_SHADER, File("shaders/fragment.glsl")))

	// Load the finalMeshStream

	val vao = glGenVertexArrays()
	val vbo = glGenBuffers()

	///////////
	// LEGUI //
	///////////

	// Firstly we need to create frame component for window.
	val frame = Frame(WIDTH.toFloat(), HEIGHT.toFloat())
	val modelToShow = zAxisAlignedCylinder
	val (paramsGuiForSymbolic, varStream) = paramsGuiForSymbolic(modelToShow)
	frame.container.add(paramsGuiForSymbolic)

	// We need to create legui context which shared by renderer and event processor.
	// Also we need to pass event processor for ui events such as click on component, key typing and etc.
	val leguiEventProcessor = EventProcessor()
	val context = Context(window, frame, leguiEventProcessor)

	// We need to create callback keeper which will hold all of callbacks.
	// These callbacks will be used in initialization of system event processor
	// (will be added callbacks which will push system events to event queue and after that processed by SystemEventProcessor)
	val keeper = DefaultCallbackKeeper()

	// register callbacks for window. Note: all previously binded callbacks will be unbinded.
	keeper.registerCallbacks(window)

	// Event processor for system events. System events should be processed and translated to gui events.
	val systemEventProcessor = SystemEventProcessor(frame, context, keeper)

	// Also we need to create renderer provider
	// and create renderer which will render our ui components.
	val provider = NvgRendererProvider.getInstance()
	RendererProvider.setRendererProvider(provider)

	// Renderer which will render our ui components.
	val renderer = NvgRenderer(context, NvgRendererProvider.getInstance())
	renderer.initialize()

	////////////////
	// Mesh stuff //
	////////////////

	val timer = interval(1000 / 60, TimeUnit.MILLISECONDS)

	val finalMeshStream = varStream.map {
		it.entries.fold(modelToShow, { symbolic, (variable, setting) ->
			symbolic.substitute(variable, ScalarC(setting))
		})
	}
			.map { it.eval() }

	val lookatStream = timer.map { time ->

		Matrix4f().lookAt(Vector3f(5.0f * Math.cos(time.toDouble() / 20.0).toFloat(),
								   5.0f * Math.sin(time.toDouble() / 20.0).toFloat(),
								   3.0f),
						  Vector3f(0.0f, 0.0f, 0.0f),
						  Vector3f(0.0f, 0.0f, 1.0f))
	}

	val projection = Matrix4f().perspective((Math.PI / 2.0).toFloat(),
											WIDTH.toFloat() / HEIGHT.toFloat(),
											0.1f, 1000.0f)

	Observables.combineLatest(finalMeshStream, lookatStream).sample(timer)
			.blockingForEach {
				(mesh, view) ->
				drawFrame(vbo,
						  mesh,
						  shaderProgram,
						  window,
						  view,
						  projection,
						  renderer,
						  frame,
						  context,
						  vao)

				//!glfwWindowShouldClose(window)
// Now we need to process events. Firstly we need to process system events.
				systemEventProcessor.processEvent()

				// When system events are translated to GUI events we need to process them.
				// This event processor calls listeners added to ui components
				leguiEventProcessor.processEvent()
			}

	renderer.destroy()

	glfwDestroyWindow(window)

	glfwTerminate()
	cb.free()

}

private fun createWindowWithContext(width: Int, height: Int): Long {
	val window = glfwCreateWindow(width, height, "Drawy Visualizer", NULL, NULL)

	if (window == NULL) {
		glfwTerminate()
		throw RuntimeException("Failed to create the GLFW window")
	}

	// Create an OpenGL context associated with the window
	glfwMakeContextCurrent(window)

	// Initialize LWJGL with the context
	createCapabilities()

	glfwSwapInterval(0)

	return window
}

private fun initGlfw(): GLFWErrorCallback {
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
	return cb
}

private fun drawFrame(vbo: Int,
					  mesh: Mesh,
					  shaderProgram: Int,
					  window: Long,
					  view: Matrix4f,
					  projection: Matrix4f,
					  renderer: Renderer,
					  frame: Frame,
					  context: Context,
					  vao: Int) {

	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

	render3D(projection, view, shaderProgram, vbo, mesh, vao)

	renderLEGUI(context, renderer, frame)

	glfwSwapBuffers(window)
	glfwPollEvents()
}

private fun render3D(projection: Matrix4f,
					 view: Matrix4f,
					 shaderProgram: Int,
					 vbo: Int,
					 mesh: Mesh,
					 vao: Int) {

	glClearColor(0.0f, 0.5f, 0.0f, 1.0f)
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
	glEnable(GL_DEPTH_TEST)

	glUseProgram(shaderProgram)

	glBindVertexArray(vao)

	val modelviewprojection = projection.mul(view, Matrix4f())

	val uniView = glGetUniformLocation(shaderProgram, "modelview")
	glUniformMatrix4fv(uniView, false, view.get(FloatArray(16)))

	val uniProjection = glGetUniformLocation(shaderProgram, "modelviewproj")
	glUniformMatrix4fv(uniProjection, false, modelviewprojection.get(FloatArray(16)))

	glBindBuffer(GL_ARRAY_BUFFER, vbo)
	glBufferData(GL_ARRAY_BUFFER,
				 mesh.vertices.zip(mesh.normals)
						 .flatMap { (v, n) -> listOf(v.x, v.y, v.z, n.x, n.y, n.z) }
						 .toFloatArray(), GL_DYNAMIC_DRAW)

	val BYTES_PER_FLOAT = 4

	val posAttrib = glGetAttribLocation(shaderProgram, "position")
	glEnableVertexAttribArray(posAttrib)
	glVertexAttribPointer(posAttrib, 3, GL_FLOAT, false, 6 * BYTES_PER_FLOAT, 0)

	val normalAttrib = glGetAttribLocation(shaderProgram, "in_normal")
	glEnableVertexAttribArray(normalAttrib)
	glVertexAttribPointer(normalAttrib, 3, GL_FLOAT, false, 6 * BYTES_PER_FLOAT,
						  (3 * BYTES_PER_FLOAT).toLong())

	glDrawArrays(GL_TRIANGLES, 0, mesh.faces.count() * 3)
}

private fun renderLEGUI(context: Context,
						renderer: Renderer,
						frame: Frame) {
	context.updateGlfwWindow()

	renderer.render(frame)
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

private fun paramsGuiForSymbolic(symbolic: Symbolic<Any>): Pair<Panel<Component>, Observable<Map<Variable<out Any>, Float>>> {

	val panel = Panel<Component>()

	val variables = symbolic.variables

	val PADDING = 20f

	val SLIDER_HEIGHT = 20f

	val PANEL_WIDTH = 200f

	val LABEL_WIDTH = 50f

	val CONTENT_WIDTH = PANEL_WIDTH - PADDING * 2

	val sliders = variables.mapIndexed { index, varib ->
		Slider(PADDING + LABEL_WIDTH,
			   index.toFloat() * SLIDER_HEIGHT + PADDING,
			   CONTENT_WIDTH - LABEL_WIDTH,
			   SLIDER_HEIGHT)
	}

	val labels = variables.mapIndexed { index, variable ->
		Label(variable.name,
			  PADDING,
			  index.toFloat() * SLIDER_HEIGHT + PADDING,
			  LABEL_WIDTH,
			  SLIDER_HEIGHT)
	}

	val variablesToInitialValues = variables.associate { Pair(it, 0f) }

	val varValStream = variables.zip(sliders).toObservable().flatMap { (variable, slider) ->

		slider.listenerMap.observableListener(SliderChangeValueEvent::class.java)
				.map { it.newValue / 10f - 5f }
				.map { Pair(variable, it) }
				.startWith(Pair(variable, 0f))

	}.scan(variablesToInitialValues,
		   { old: Map<Variable<out Any>, Scalar>,
			 update: Pair<Variable<out Any>, Scalar> ->
			   old.plus(update)
		   })

	panel.addAll(labels.zip(sliders).flatMap { it.toList() })

	panel.setSize(PANEL_WIDTH, variables.size * SLIDER_HEIGHT + 2 * PADDING)

	return Pair(panel, varValStream)
}

fun <E : Event<out Component>> ListenerMap.observableListener(eventclass: Class<E>): Observable<E> {
	val subject = PublishSubject.create<E>()

	this.addListener(eventclass, {
		subject.onNext(it)
	})

	return subject
}