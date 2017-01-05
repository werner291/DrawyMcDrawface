package nl.wernerkroneman.Drawy.Algorithms;

import java.util.*;

/**
 * An implementation of Kahn's algorithm for topological sorting.
 */
public class TopologicalSort {
    /**
     * Execute Kahn's algorithm on a collection of nodes.
     *
     * @param nodes        A collection of nodes
     * @param dependencies For every node in {@code nodes}, a list of nodes that it depends on
     * @param <Node>       The type of Nodes.
     *                     <p>
     *                     Modifies: {@code dependants, dependencies}
     * @return List of nodes such that, for any node in the list, all of that node's dependencies appear earlier in
     * the list.
     */
    public static <Node> List<Node> topologicalSort(Collection<Node> nodes, Map<Node,
            List<Node>> dependencies) {

        Map<Node, List<Node>> dependants = new HashMap<Node, List<Node>>();

        for (Node node : nodes) {
            dependants.put(node, new ArrayList<Node>());
        }

        for (Map.Entry<Node, List<Node>> nodeDependencies : dependencies.entrySet()) {
            for (Node dependency : nodeDependencies.getValue()) {
                dependants.get(dependency).add(nodeDependencies.getKey());
            }
        }

        // Put all nodes without dependencies in the list.
        Queue<Node> noIncoming = new LinkedList<Node>();

        for (Node comp : nodes) {
            if (dependencies.get(comp).isEmpty()) {
                noIncoming.add(comp);
            }
        }

        List<Node> result = new ArrayList<>();

        while (!noIncoming.isEmpty()) {
            // Get a node with no incoming arrows
            Node comp = noIncoming.poll();

            // Add to tail of the list
            result.add(comp);

            for (Node dependant : dependants.get(comp)) {
                // Remove it as a dependency
                dependencies.get(dependant).remove(comp);
                if (dependencies.get(dependant).isEmpty()) {
                    noIncoming.add(dependant);
                }
            }

            dependencies.remove(comp);
        }

        if (result.size() < nodes.size()) {
            throw new IllegalStateException("Constraints not solvable.");
        }

        return result;
    }
}
