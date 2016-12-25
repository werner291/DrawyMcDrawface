#include <gtest/gtest.h>
#include "../src/AbstractSceneModel.h"
#include "../src/Interpreter.h"
#include "../src/SceneCommand.h"
#include "../src/Knowledge.h"

TEST(SceneCommandTest, creationDoubleApplicationTest) {

    std::shared_ptr<AbstractSceneModel> model = std::make_shared<AbstractSceneModel>();

    // Verify empty scene
    ASSERT_TRUE(model->entities.empty());

    CreateEntityRule stmt(model);
    stmt.what = "box";
    stmt.number = 5;

    stmt.apply();

    ASSERT_EQ(1, model->entities.size());

    ASSERT_THROW(stmt.apply(), std::runtime_error);

}