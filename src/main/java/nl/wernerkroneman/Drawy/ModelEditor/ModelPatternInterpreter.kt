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

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.CreateCommandInterpreter
import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.DistanceInterpreter
import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.ModelInterpreter
import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.RelativePositionInterpreter
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.BELOW
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePatternBuilder
import nl.wernerkroneman.Drawy.ParseTreeMatcher.buildPattern

/**
 * Created by werner on 8-3-17.
 */
fun createDefaultModelInterpreter(): PatternInterpreter {

    val interpreter = PatternInterpreter()

    val knowledge = Knowledge.knowledgeWithPrimitives()

    val modelInterpreter = ModelInterpreter(knowledge, interpreter)

    val createCommandInterpreter = CreateCommandInterpreter(interpreter)

    val distanceInterpreter = DistanceInterpreter()

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .role("ROOT")
                    .nature("NN")
                    .name("what")
                    .create())

    interpreter.addPattern(modelInterpreter,
            PhrasePatternBuilder()
                    .nature("NN")
                    .name("name")
                    .create())

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .word("Give")
                    .child(PhrasePatternBuilder()
                            .word("me")
                            .create())
                    .child(PhrasePatternBuilder()
                            .name("what")
                            .create())
                    .child(PhrasePatternBuilder()
                            .role("punct")
                            .create()
                    ).create())

    interpreter.addPattern(createCommandInterpreter,
            PhrasePatternBuilder()
                    .word("Create")
                    .child(PhrasePatternBuilder()
                            .name("what")
                            .create())
                    .child(PhrasePatternBuilder()
                            .role("punct")
                            .create()
                    ).create())

    interpreter.addPattern(RelativePositionInterpreter(ABOVE, interpreter),
            PhrasePatternBuilder()
                    .role("prep")
                    .word("above")
                    .children(
                            buildPattern {
                                role("npadvmod")
                                name("distance")
                                optional()},
                            PhrasePatternBuilder()
                                    .role("pobj")
                                    .name("relative_to")
                                    .create()
                    ).create())

    interpreter.addPattern(RelativePositionInterpreter(BELOW, interpreter),
            PhrasePatternBuilder()
                    .role("prep")
                    .word("below")
                    .children(
                            PhrasePatternBuilder()
                                    .role("pobj")
                                    .name("relative_to")
                                    .create()
                    ).create())

    interpreter.addPattern(distanceInterpreter,
            buildPattern {
                nature("NN*")
                child( buildPattern {
                            nature("CD")
                            role("num")
                            name("amount")
                })
            })

    return interpreter

}