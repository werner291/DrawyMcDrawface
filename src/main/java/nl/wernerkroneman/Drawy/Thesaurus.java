package nl.wernerkroneman.Drawy;

import org.apache.commons.io.IOUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class Thesaurus {

    final String API_KEY = "TODO: PUT THIS SOMEWHERE YOU CANNOT READ THIS";

    Map<String,Collection<String> > cache = new HashMap<>();

    boolean isSynonimOf(String word, String synonimOf) {
        return getSynonimsOf(word).contains(synonimOf);
    }

    public Collection<String> getSynonimsOf(String word) {

        Collection<String> synonims = cache.get(word);

        if (synonims != null) {
            return synonims;
        }

        try {
            URL url = new URL("http://words.bighugelabs.com/api/2/"+API_KEY+"/"
                    + URLEncoder.encode(word,"utf8")
                    +"/json");

            String json = IOUtils.toString(url.openStream());

            JSONParser obj = new JSONParser();

            JSONObject result = (JSONObject) obj.parse(json);

            synonims = (List<String>) (result.values().stream()
                    .map(e -> ((JSONObject)e).get("syn"))
                    .flatMap(e -> ((JSONArray)e).stream())
                    .collect(Collectors.toList()));

            cache.put(word, synonims);

            return synonims;

        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ParseException e) {
            e.printStackTrace();
        }

        return null;
    }



}
