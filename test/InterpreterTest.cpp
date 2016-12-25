#include <gtest/gtest.h>
#include "../src/Interpreter.h"

TEST(InterpreterTests, interpreterTest1) {

    std::shared_ptr<AbstractSceneModel> model = std::make_shared<AbstractSceneModel>();
    Knowledge knowledge;

    std::vector<std::shared_ptr<SceneCommand> > result = interpret("Create a box.", model, knowledge, false);

    std::shared_ptr<CreateEntityRule> stmt;

    ASSERT_TRUE(stmt = std::dynamic_pointer_cast<CreateEntityRule>(result[0]));

    ASSERT_EQ(1, stmt->number);

    ASSERT_EQ("box", stmt->what);

}

TEST(InterpreterTests, interpreterTest2) {

    std::shared_ptr<AbstractSceneModel> model = std::make_shared<AbstractSceneModel>();
    Knowledge knowledge;

    std::vector<std::shared_ptr<SceneCommand> > result = interpret("Add a palm tree or two.", model, knowledge, false);

    std::shared_ptr<CreateEntityRule> stmt;

    ASSERT_TRUE(stmt = std::dynamic_pointer_cast<CreateEntityRule>(result[0]));

    ASSERT_LE(1, stmt->number);
    ASSERT_GE(2, stmt->number);

    ASSERT_EQ("tree", stmt->what);

}

TEST(InterpreterTests, createWithAnd) {

    std::shared_ptr<AbstractSceneModel> model = std::make_shared<AbstractSceneModel>();
    Knowledge knowledge;

    std::vector<std::shared_ptr<SceneCommand> > result = interpret("Add a box and a cylinder", model, knowledge, false);

    ASSERT_EQ(2, result.size());

    std::shared_ptr<CreateEntityRule> stmt;

    ASSERT_TRUE(stmt = std::dynamic_pointer_cast<CreateEntityRule>(result[0]));

    ASSERT_EQ(1, stmt->number);

    ASSERT_EQ("box", stmt->what);

    ASSERT_TRUE(stmt = std::dynamic_pointer_cast<CreateEntityRule>(result[1]));

    ASSERT_EQ(1, stmt->number);

    ASSERT_EQ("cylinder", stmt->what);

}