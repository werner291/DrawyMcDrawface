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

package nl.wernerkroneman.Drawy;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Collection;

/**
 * Created by werner on 5-1-17.
 */
public class ThesaurusTest {
    @Test @Ignore("Need to get API key management and opensource stuff figured out.")
    public void getSynonimsOf() throws Exception {

        Thesaurus thesaurus = new Thesaurus();

        Collection<String> synonims = thesaurus.getSynonimsOf("below");

        Assert.assertTrue(synonims.contains("under"));
        Assert.assertTrue(synonims.contains("beneath"));

    }

}