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

        List<String> result = TopologicalSort.INSTANCE.topologicalSort(strings, dependencies);

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