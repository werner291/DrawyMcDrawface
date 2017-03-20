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

import java.util.*

/**
 * Execute Kahn's algorithm on a collection of nodes.

 * @param nodes        A collection of nodes
 *
 * @param dependencies For every node in `nodes`, a list of nodes that it depends on
 *
 * @param <Node>       The type of Nodes.
 *
 *                     Modifies: `dependants, dependencies`
 *
 * @return List of nodes such that, for any node in the list,
 *         all of that node's dependencies appear earlier in
 *         the list.
 */
fun <Node> topologicalSort(nodes: Iterable<Node>,
                           dependencyFinder: (Node) -> Iterable<Node>): List<Node> {

    val expectedNumber = nodes.count()

    val dependencies = nodes.associateTo(HashMap<Node, MutableSet<Node>>(),
            { Pair(it, dependencyFinder(it).toMutableSet()) })

    val dependants = nodes.associateTo(HashMap<Node, MutableSet<Node>>(),
            { dependency ->
                Pair(dependency, dependencies.filterValues { it.contains(dependency) }
                        .keys.toMutableSet())
            })

    val queue = dependencies.filter { it.value.isEmpty() }.keys.toMutableList()

    val result = mutableListOf<Node>()

    while (!queue.isEmpty()) {

        // Get a node with no incoming arrows
        val node = queue.removeAt(0)

        // Add to tail of the list
        result.add(node)

        dependants[node]!!.forEach {
            // Remove it as a dependency
            dependencies[it]!!.remove(node)
            if (dependencies[it]!!.isEmpty()) {
                queue.add(it)
            }
        }
    }

    if (result.size < expectedNumber) {
        throw IllegalStateException("Dependencies contain at least one cycle.")
    }

    return result
}
