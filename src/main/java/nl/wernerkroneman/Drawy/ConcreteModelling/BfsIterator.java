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

    Queue<SceneNode> queue = new ArrayDeque<>();

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
