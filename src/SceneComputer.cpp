/*
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) 2016  Werner Kroneman <email>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "SceneComputer.h"

SceneComputer::SceneComputer(AbstractSceneModel &defintion)
    :definition(defintion)
{
    definition.createEntity("primitive", "cube");
    definition.createEntity("primitive", "cylinder");

    definition.createEntity("primitive", "Current Scene");
}

void SceneComputer::processEntity(EntityPtr entModel, Ogre::SceneManager* mSceneMgr)
{

    Ogre::SceneManager::PrefabType pfType;

    // Determine shape
    if (entModel->findAncestor("cube").lock().get() != nullptr)
    {
        pfType = Ogre::SceneManager::PT_CUBE;
        std::cout << "Creating cube ";
    }
    else
    {
        pfType = Ogre::SceneManager::PT_SPHERE;
        std::cout << "Creating sphere ";
    }

    // Create a prefab cube
    Ogre::Entity *planeEnt = mSceneMgr->createEntity(pfType);

    // Give the plane a texture
    planeEnt->setMaterialName("DefaultGray");

    // Determine position (moving other entities if needed)

    float radius = planeEnt->getBoundingRadius();
    std::uniform_real_distribution< float > positionGen(-radius,radius);

    Ogre::Vector3 pos;
    Ogre::AxisAlignedBox candidateBounds;
    bool placed = true;

    do
    {
        pos = Ogre::Vector3(positionGen(rando),positionGen(rando),positionGen(rando));
        placed = true;

        candidateBounds = Ogre::AxisAlignedBox(
            Ogre::Vector3(pos - planeEnt->getBoundingRadius()),
            Ogre::Vector3(pos + planeEnt->getBoundingRadius())
        );

        std::cout << "Candidate: " << candidateBounds << std::endl;

        for (Ogre::AxisAlignedBox otherBox : occupiedSpace)
        {
            if (otherBox.intersects(candidateBounds))
            {
                std::cout << "Intersect found: " << otherBox << " - " << radius << std::endl;
                placed = false;
                radius *= 1.1;
                positionGen = std::uniform_real_distribution< float >(-radius,radius);
                break;;
            }
            else
            {
                std::cout << "Clear." << std::endl;
            }
        }

    }
    while (!placed);
    
    occupiedSpace.push_back(candidateBounds);

    Ogre::SceneNode* node = mSceneMgr->getRootSceneNode()
                            ->createChildSceneNode(pos);

    // Attach the 2 new entities to the root of the scene

    node->attachObject(planeEnt);

    std::cout << node->getPosition() << std::endl;


}