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

#ifndef SCENECOMPUTER_H
#define SCENECOMPUTER_H

/**
 * The element in the pipeline between the SceneContainer
 * and the Renderer.
 * 
 * It turns the "constrains" and "Entity Definitions"
 * into something much more concrete that the renderer
 * can directly translate into an Ogre scene.
 */
class SceneComputer
{
public:
    SceneComputer();
};

#endif // SCENECOMPUTER_H
