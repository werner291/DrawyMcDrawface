/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy

/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

//import nl.wernerkroneman.Drawy.Interface.BlockingInteractor;
//import nl.wernerkroneman.Drawy.Interface.UserInteractor;
//import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSessionListener;
//import nl.wernerkroneman.Drawy.ModelEditor.Knowledge;
//import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession;

//import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.ABOVE;

import com.jogamp.opengl.GLCapabilities
import com.jogamp.opengl.GLProfile
import com.jogamp.opengl.awt.GLCanvas
import nl.wernerkroneman.Drawy.AbstractToConcreteConverter.AbstractToConcrete
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene
import nl.wernerkroneman.Drawy.GlRenderer.GlVisualiser
import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.Interface.UserInteractor
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.ModelEditor.MainInterpreter
import nl.wernerkroneman.Drawy.ModelEditor.createDefaultModelInterpreter
import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.FixedDistance
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import org.joml.Vector3d
import java.awt.BorderLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.JFrame
import javax.swing.Timer
import javax.swing.WindowConstants

fun main(args: Array<String>) {

    val frame = JFrame("DrawyMcDrawface")

    val glprofile = GLProfile.getGL2GL3()
    val glcapabilities = GLCapabilities(glprofile)
    val glcanvas = GLCanvas(glcapabilities)

    frame.contentPane.add(glcanvas)

    frame.defaultCloseOperation = WindowConstants.EXIT_ON_CLOSE

    frame.setSize(800, 600)

    val interactor = UserInteractor()
    frame.contentPane.add(interactor, BorderLayout.SOUTH)

    frame.isVisible = true

    frame.pack()

    // Initialize empty knowledge
    val knowledge = Knowledge.knowledgeWithPrimitives()

    knowledge.remember(createSnowman(knowledge))

    println("Loaded " + knowledge.numberOfObjects + " concepts. ")

    val iface = BlockingInteractor(interactor)
    val session = DescriptionSession(
            interpreter = MainInterpreter(createDefaultModelInterpreter(knowledge)),
            interactorIface = iface)

    // Create a visualizer
    val visualiser = GlVisualiser()

    glcanvas.addGLEventListener(visualiser)

    // Create a converter that uses meshes compatible with the Renderer
    val converter = AbstractToConcrete(visualiser.meshFactory)

    // Every time changes are made to the scene
    session.addChangeListener {scene -> // Convert it to a concrete scene
        val concreteScene = converter.computeScene(scene)

        // Pass it to
        visualiser.scene = concreteScene

        glcanvas.display()}

    print("Main scene" + ">: ")

    val timer = Timer(50, object : ActionListener {
        internal var t = 0.0

        override fun actionPerformed(e: ActionEvent) {
            t += 0.01
            val box = visualiser.scene.rootSceneNode.computeWorldAABB()
            val d = 5 * Math.max(box.sizeX, box.sizeZ)
            Scene.EYE.set(d * Math.sin(t), 10.0 * Math.sin(t), d * Math.cos(t))
            Scene.LOOKAT_CENTER.set(box.getCenter(Vector3d()))
            glcanvas.display()
        }
    })
    timer.start()

    session.start()
}

private fun createSnowman(knowledge: Knowledge): CompositeModel {
    val snowman = CompositeModel("snowman")

    val head = snowman.createComponentForModel(knowledge.getObject("sphere")!!)
    val body = snowman.createComponentForModel(knowledge.getObject("sphere")!!)
    snowman.constraints.add(RelativePositionConstraint(head, body, ABOVE, FixedDistance(0.0)))
    return snowman
}

/*private static class MyDescriptionSessionListener implements DescriptionSessionListener {
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
}*/

