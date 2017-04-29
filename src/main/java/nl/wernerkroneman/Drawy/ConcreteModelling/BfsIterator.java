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

package nl.wernerkroneman.Drawy.ConcreteModelling;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Queue;

/**
 * An iterator that takes a SceneNode as input,
 * and allows for iteration over the tree rooted
 * at this node in breadth-first order.
 */
class BfsIterator implements Iterator<SceneNode> {

    private final Queue<SceneNode> queue = new ArrayDeque<>();

    public BfsIterator(SceneNode root) {
        queue.add(root);
    }

    @Override
    public boolean hasNext() {
        return !queue.isEmpty();
    }

    @Override
    public SceneNode next() {
        SceneNode node = queue.poll();

        if (node == null) {
            throw new NoSuchElementException("There are no more elements!");
        }

        queue.addAll(node.getChildren());

        return node;
    }
}
