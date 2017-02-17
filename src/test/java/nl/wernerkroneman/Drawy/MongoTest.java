package nl.wernerkroneman.Drawy;

import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import org.junit.Test;

import java.net.UnknownHostException;

/**
 * Created by werner on 20-1-17.
 */
public class MongoTest {

    @Test
    public void mongoTest() throws Exception {

        MongoClient mongoClient = new MongoClient(new MongoClientURI("mongodb://localhost:27017"));

        DB database = mongoClient.getDB("TheDatabaseName");

        //collection.
    }
}
