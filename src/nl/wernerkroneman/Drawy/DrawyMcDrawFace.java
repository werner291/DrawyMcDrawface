package nl.wernerkroneman.Drawy;/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

import nl.wernerkroneman.Drawy.ConcreteModelling.AbstractToConcrete;
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene;
import nl.wernerkroneman.Drawy.GlRenderer.GlVisualiser;
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession;
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge;

public class DrawyMcDrawFace {

    public static void main(String[] args) {

        // Initialize empty knowledge
        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        System.out.println("Loaded " + knowledge.getNumberOfObjects() + " concepts. ");

        // Create a description session with a root scene object
        DescriptionSession session = new DescriptionSession();

        // Create a visualizer
        GlVisualiser visualiser = new GlVisualiser();

        // Create a converter that uses meshes compatible with the Renderer
        AbstractToConcrete converter = new AbstractToConcrete(visualiser.getMeshFactory());

        // Every time changes are made to the scene
        session.addListener(scene -> {
            // Convert it to a concrete scene
            Scene concreteScene = converter.computeScene(scene);

            // Pass it to
            visualiser.setScene(concreteScene);

            visualiser.show();
        });

        session.descriptionSession("Main scene", knowledge);
    }
}
