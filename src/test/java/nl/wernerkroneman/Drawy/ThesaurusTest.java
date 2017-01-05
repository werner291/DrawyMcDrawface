package nl.wernerkroneman.Drawy;

import nl.wernerkroneman.Drawy.Thesaurus;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Collection;

import static org.junit.Assert.*;

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