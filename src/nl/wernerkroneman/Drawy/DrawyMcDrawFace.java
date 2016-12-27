package nl.wernerkroneman.Drawy;/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession;
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge;

public class DrawyMcDrawFace {

    public static void main(String[] args) {

        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        System.out.println("Loaded " + knowledge.getNumberOfObjects() + " concepts. ");

        DescriptionSession session = new DescriptionSession();

        session.descriptionSession("Main scene", knowledge);
    }
}
