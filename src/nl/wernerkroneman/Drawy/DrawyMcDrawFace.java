package nl.wernerkroneman.Drawy;/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

import com.jogamp.opengl.GLCapabilities;
import com.jogamp.opengl.GLProfile;
import com.jogamp.opengl.awt.GLCanvas;
import nl.wernerkroneman.Drawy.AbstractToConcreteConverter.AbstractToConcrete;
import nl.wernerkroneman.Drawy.ConcreteModelling.AABB;
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene;
import nl.wernerkroneman.Drawy.GlRenderer.GlVisualiser;
import nl.wernerkroneman.Drawy.ModelEditor.BlockingInteractorInterface;
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession;
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSessionListener;
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge;
import nl.wernerkroneman.Drawy.Modelling.Model;
import org.joml.Vector3d;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class DrawyMcDrawFace {

    public static void main(String[] args) {
        JFrame frame = new JFrame();

        frame.setTitle("DrawyMcDrawface");

        GLProfile glprofile = GLProfile.getGL2GL3();
        GLCapabilities glcapabilities = new GLCapabilities(glprofile);
        final GLCanvas glcanvas = new GLCanvas(glcapabilities);

        frame.getContentPane().add(glcanvas);

        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        frame.setSize(800, 600);

        UserInteractor interactor = new UserInteractor();
        frame.getContentPane().add(interactor, BorderLayout.SOUTH);

        frame.setVisible(true);

        // Initialize empty knowledge
        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        System.out.println("Loaded " + knowledge.getNumberOfObjects() + " concepts. ");

        BlockingInteractorInterface iface = new BlockingInteractorInterface(interactor);
        DescriptionSession session = DescriptionSession.createDescriptionSession(knowledge, iface);

        // Create a visualizer
        GlVisualiser visualiser = new GlVisualiser();

        glcanvas.addGLEventListener(visualiser);

        // Create a converter that uses meshes compatible with the Renderer
        AbstractToConcrete converter = new AbstractToConcrete(visualiser.getMeshFactory());

        // Every time changes are made to the scene
        session.addListener(new MyDescriptionSessionListener(converter, visualiser, glcanvas));

        System.out.print("Main scene" + ">: ");

        Timer timer = new Timer(50, new ActionListener() {
            double t = 0;

            @Override
            public void actionPerformed(ActionEvent e) {
                t += 0.01;
                AABB box = visualiser.getScene().getRootSceneNode().computeWorldAABB();
                double d = 10 * Math.max(box.getWidth(), box.getDepth());
                Scene.EYE.set(d * Math.sin(t), 10, d * Math.cos(t));
                Scene.LOOKAT_CENTER.set(box.getCenter(new Vector3d()));
                glcanvas.display();
            }
        });
        timer.start();

        session.start();
    }

    private static class MyDescriptionSessionListener implements DescriptionSessionListener {
        private final AbstractToConcrete converter;
        private final GlVisualiser visualiser;
        private final GLCanvas glcanvas;

        public MyDescriptionSessionListener(AbstractToConcrete converter, GlVisualiser visualiser, GLCanvas glcanvas) {
            this.converter = converter;
            this.visualiser = visualiser;
            this.glcanvas = glcanvas;
        }

        @Override
        public void modelChanged(Model scene) {
            // Convert it to a concrete scene
            Scene concreteScene = converter.computeScene(scene);

            // Pass it to
            visualiser.setScene(concreteScene);

            glcanvas.display();
        }

        @Override
        public void sessionEnded() {

        }

        @Override
        public void recursiveSessionStarted(DescriptionSession recursiveSession) {

        }
    }
}
