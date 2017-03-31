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

package nl.wernerkroneman.Drawy.Algorithms

import org.junit.Assert
import org.junit.Test
import java.util.*


class TopologicalSortTest {
    @Test
    @Throws(Exception::class)
    fun topologicalSort() {

        val strings = HashSet<String>()

        strings.add("underpants")
        strings.add("pants")
        strings.add("socks")
        strings.add("shoes")
        strings.add("watch")
        strings.add("pull-over")
        strings.add("coat")
        strings.add("shirt")

        val dependencies = HashMap<String, MutableList<String>>()

        for (str in strings) {
            dependencies.put(str, ArrayList<String>())
        }

        dependencies["coat"]!!.add("pull-over")
        dependencies["pull-over"]!!.add("shirt")
        dependencies["pants"]!!.add("underpants")
        dependencies["shoes"]!!.add("socks")

        val result = topologicalSort(strings, { dependencies[it]!! })

        for (str in strings) {
            Assert.assertTrue(result.contains(str))
        }

        for ((key, value) in dependencies) {

            for (dependency in value) {
                Assert.assertTrue(result.indexOf(dependency) < result.indexOf(key))
            }

        }

    }

}