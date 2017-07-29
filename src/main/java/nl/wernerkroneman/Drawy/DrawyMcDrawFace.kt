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

import com.jogamp.opengl.GLCapabilities
import com.jogamp.opengl.GLProfile
import com.jogamp.opengl.awt.GLCanvas
import io.reactivex.Observable
import io.reactivex.Single
import io.reactivex.Single.just
import nl.wernerkroneman.Drawy.AbstractToConcreteConverter.AbstractToConcrete
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene
import nl.wernerkroneman.Drawy.GlRenderer.GlVisualiser
import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.Interface.UserInteractor
import nl.wernerkroneman.Drawy.Modelling.ModelSpecification
import java.awt.BorderLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.concurrent.TimeUnit
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

    val utilityPole = ModelSpecification()

    val abstractScene = just(utilityPole)

    val timer = Observable.interval(1000 / 60, TimeUnit.MILLISECONDS)

    val iface = BlockingInteractor(interactor)
    //val session = DescriptionSession(
    //       interpreter = defaultModelInterpreter,
    //       interactorIface = iface)

    // Create a visualizer
    val visualiser = GlVisualiser()

    glcanvas.addGLEventListener(visualiser)


    // Every time changes are made to the scene
    abstractScene
            .map({ Scene(AbstractToConcrete(visualiser.meshFactory).concreteForModel(it)) })
            .subscribe { scene -> visualiser.scene = scene; glcanvas.display() }


    /*internal var t = 0.0

    override fun actionPerformed(e: ActionEvent) {
        t += 0.05
        val box = visualiser.scene.rootSceneNode.aabb
        val d = 5 * Math.max(box.sizeX, box.sizeZ)
        Scene.EYE.set(d * Math.sin(t), 5.0 * Math.sin(t), d * Math.cos(t))
        Scene.LOOKAT_CENTER.set(box.center)
        glcanvas.display()
    }*/
}

