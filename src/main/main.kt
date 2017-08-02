import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL.*
import org.lwjgl.system.MemoryUtil.NULL

fun main(args : Array<String>) {

	val cb = GLFWErrorCallback.createPrint(System.err);

	glfwSetErrorCallback(cb)

	if (!glfwInit()) {
		throw IllegalStateException("Unable to initialize GLFW")
	}

	val window = glfwCreateWindow(1024,768, "Drawy Visualizer", NULL, NULL)

	if (window == NULL) {
		glfwTerminate()
		throw RuntimeException("Failed to create the GLFW window")
	}

	glfwMakeContextCurrent(window);
	createCapabilities();

	while (!glfwWindowShouldClose(window)) {
		glfwSwapBuffers(window);
		glfwPollEvents();
	}

	glfwDestroyWindow(window);

	glfwTerminate();
	cb.free();

}
