package nl.wernerkroneman.Drawy.Algorithms;

import org.junit.Assert;
import org.junit.Test;

import java.util.*;


public class TopologicalSortTest {
    @Test
    public void topologicalSort() throws Exception {

        Set<String> strings = new HashSet<>();

        strings.add("underpants");
        strings.add("pants");
        strings.add("socks");
        strings.add("shoes");
        strings.add("watch");
        strings.add("pull-over");
        strings.add("coat");
        strings.add("shirt");

        Map<String, List<String>> dependencies = new HashMap<>();

        for (String str : strings) {
            dependencies.put(str, new ArrayList<>());
        }

        dependencies.get("coat").add("pull-over");
        dependencies.get("pull-over").add("shirt");
        dependencies.get("pants").add("underpants");
        dependencies.get("shoes").add("socks");

        List<String> result = TopologicalSort.topologicalSort(strings, dependencies);

        for (String str : strings) {
            Assert.assertTrue(result.contains(str));
        }

        for (Map.Entry<String, List<String>> entry : dependencies.entrySet()) {

            for (String dependency : entry.getValue()) {
                Assert.assertTrue(result.indexOf(dependency) < result.indexOf(entry.getKey()));
            }

        }

    }

}