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

package nl.wernerkroneman.Drawy.ModelEditor.ScenarioTests

import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import org.junit.Test

@Test
fun createSolarSystem() {

    // Nearly-raw copypasta from https://simple.wikipedia.org/wiki/Solar_System
    val result = runScript(listOf(
            "The Solar System is the Sun and all the objects in orbit around it.",
            "The Sun is orbited by planets, asteroids, comets and other things.",
            "The Sun is a star.",
            "It contains 99.9 percent of the Solar System's mass.",
            "This means that it has strong gravity.",
            "The other objects are pulled into orbit around the Sun.",
            "The sun is mostly made out of hydrogen and helium.",
            "There are eight planets in the Solar System.",
            "From closest to farthest from the Sun, they are: Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus and Neptune.",
            "The first four planets are called terrestrial planets.",
            "They are mostly made of rock and metal, and they are mostly solid.",
            "The last four planets are called gas giants.",
            "This is because they are much larger than other planets and are mostly made of gas.",
            "The Solar System also contains other things.",
            "There are asteroids, mostly between Mars and Jupiter.",
            "Further out than Neptune, there is the Kuiper belt and the scattered disc.",
            "These areas have dwarf planets, including Pluto.",
            "There are thousands of very small objects in these areas.",
            "There are also comets, centaurs, and there is interplanetary dust.",
            "Six of the planets and three of the dwarf planets are orbited by moons.",
            "Furthermore, planetary dust orbits the gas giants."
    ), Knowledge.knowledgeWithPrimitives())


}
