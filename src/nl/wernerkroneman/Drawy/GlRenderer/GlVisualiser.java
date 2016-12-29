package nl.wernerkroneman.Drawy.GlRenderer;

import com.jogamp.newt.event.WindowAdapter;
import com.jogamp.newt.event.WindowEvent;
import com.jogamp.newt.opengl.GLWindow;
import com.jogamp.opengl.*;
import nl.wernerkroneman.Drawy.ConcreteModelling.MeshFactory;
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene;

public class GlVisualiser implements GLEventListener {
    private static final int WINDOW_WIDTH = 640;  // width of the drawable
    private static final int WINDOW_HEIGHT = 480; // height of the drawable
    private static String TITLE = "Drawy!";  // window's title
    private final GLWindow window;
    private Scene scene = new Scene(); // Empty scene
    private MeshFactory meshFactory;


    private Renderer renderer = new Renderer();

    /**
     * Constructor
     */
    public GlVisualiser() {// Get the default OpenGL profile, reflecting the best for your running platform
        GLProfile glp = GLProfile.getGL2GL3();
        // Specifies a set of OpenGL capabilities, based on your profile.
        GLCapabilities caps = new GLCapabilities(glp);
        // Create the OpenGL rendering canvas
        window = GLWindow.create(caps);

        window.addWindowListener(new WindowAdapter() {
            @Override
            public void windowDestroyNotify(WindowEvent arg0) {
                // Use a dedicate thread to run the stop() to ensure that the
                // animator stops before program exits.
                new Thread() {
                    @Override
                    public void run() {
                        System.exit(0);
                    }
                }.start();
            }

        });

        window.addGLEventListener(this);
        window.setSize(WINDOW_WIDTH, WINDOW_HEIGHT);
        window.setTitle(TITLE);
        window.setVisible(true);
    }

    /**
     * Called back by the drawable to render OpenGL graphics
     */
    @Override
    public void display(GLAutoDrawable drawable) {
        renderer.render(scene, drawable.getGL().getGL3());
        window.swapBuffers(); // swap the color buffers to show the new image
    }

    /**
     * Called back immediately after the OpenGL context is initialized
     */
    @Override
    public void init(GLAutoDrawable drawable) {

        GL3 gl3 = drawable.getGL().getGL3();

        renderer.initShaders(gl3);
    }

    /**
     * Called back before the OpenGL context is destroyed.
     */
    @Override
    public void dispose(GLAutoDrawable drawable) {
    }

    /**
     * Called back by the drawable when it is first set to visible,
     * and during the first repaint after the it has been resized.
     */
    @Override
    public void reshape(GLAutoDrawable drawable, int x, int y, int weight, int height) {
    }

    public void setScene(Scene scene) {
        this.scene = scene;
    }

    public void show() {
        window.display();
    }

    public MeshFactory getMeshFactory() {
        return new GlMeshFactory();
    }
}