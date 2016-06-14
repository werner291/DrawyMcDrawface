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

#include "Renderer.h"

Renderer::Renderer(SceneContainer& container)
  : sceneBuilder(container)
{
  
  startOgre();
  
}

void Renderer::startOgre()
{
  mRoot = new Ogre::Root(mPluginsCfg);

    if(!(mRoot->restoreConfig() || mRoot->showConfigDialog()))
        exit(EXIT_FAILURE);


    mRenderWindow = mRoot->initialise(true, "Drawy McDrawFace");

    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);

    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();

    mSceneMgr = mRoot->createSceneManager(Ogre::ST_GENERIC);


    mCamera = mSceneMgr->createCamera("Main Camera");

    mCamera->setPosition(0, 150, 500);
    mCamera->lookAt(0, 0, 0);
    mCamera->setNearClipDistance(5);

    vp = mRenderWindow->addViewport(mCamera);

    vp->setBackgroundColour(Ogre::ColourValue(0,0,0));

    mCamera->setAspectRatio(
        Ogre::Real(vp->getActualWidth()) /
        Ogre::Real(vp->getActualHeight()));
}

void Renderer::startRendering()
{
  
  for (const Entity& ent : sceneBuilder.entities) {
      
      Ogre::SceneManager::PrefabType pfType;
      
      if (ent.isSubOf("cube")
      
      // Create a prefab cube
    Ogre::Entity *planeEnt = mSceneMgr->createEntity( Ogre::SceneManager::PT_CUBE);

        // Give the plane a texture
        planeEnt->setMaterialName("DefaultWhite");

        // Attach the 2 new entities to the root of the scene
        mSceneMgr->getRootSceneNode()
		 ->createChildSceneNode(ent.position)
		 ->attachObject(planeEnt);
    }
    
    mSceneMgr->setAmbientLight(Ogre::ColourValue(0, .5, 0));

    Ogre::Light* light = mSceneMgr->createLight("MainLight");
    light->setPosition(20, 80, 50);

    bool running = true;
    while(running)
    {
        Ogre::WindowEventUtilities::messagePump();

        if(mRenderWindow->isClosed()) break;

        if(!mRoot->renderOneFrame()) return 1;

        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
}

Renderer::~Renderer()
{
  delete mRoot;
}
