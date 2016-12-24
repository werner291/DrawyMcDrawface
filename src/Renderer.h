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

#ifndef RENDERER_H
#define RENDERER_H

#include <OGRE/Ogre.h>

#include "SceneComputer.h"

/**
 * Renderer:
 * 
 * Turn the computed concrete scene from the SceneComputer
 * into something Ogre understands.
 */
class Renderer
{
  
  #ifdef _DEBUG
const Ogre::String mResourcesCfg = "resources_d.cfg";
const Ogre::String mPluginsCfg = "plugins_d.cfg";
#else
const Ogre::String mResourcesCfg = "resources.cfg";
const Ogre::String mPluginsCfg = "plugins.cfg";
#endif

Ogre::Root* mRoot;
Ogre::RenderWindow* mRenderWindow;
Ogre::SceneManager* mSceneMgr;
Ogre::Camera* mCamera;
Ogre::Viewport* vp;
  
  SceneComputer sceneBuilder;
  
  void startOgre();
  
public:
    Renderer(SceneComputer& container);
    
    void startRendering();
    
    ~Renderer();
};

#endif // RENDERER_H
