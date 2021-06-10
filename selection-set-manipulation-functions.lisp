;;;; selection-set-manipulation-functions

(in-package #:vlisp)

"
* Selection set manipulation functions
| Function                                           | Description                                                                       |
|----------------------------------------------------+-----------------------------------------------------------------------------------|
| (ssadd [ename [ss]])                               | Adds an object (entity) to a selection set, or creates a new selection set        |
| (ssdel ename ss)                                   | Deletes an object (entity) from a selection set                                   |
| (ssget [mode] [pt1 [pt2]] [pt-list] [filter-list]) | Prompts the user to select objects (entities), and returns a selection set        |
| (ssgetfirst)                                       | Determines which objects are selected and gripped                                 |
| (sslength ss)                                      | Returns an integer containing the number of objects (entities) in a selection set |
| (ssmemb ename ss)                                  | Tests whether an object (entity) is a member of a selection set                   |
| (ssname ss index)                                  | Returns the object (entity) name of the indexed element of a selection set        |
| (ssnamex ss index)                                 | Retrieves information about how a selection set was created                       |
| (sssetfirst gripset [pickset])                     | Sets which objects are selected and gripped                                       |
"

(defun ssadd (&optional ename ss)
  " Adds an object (entity) to a selection set, or creates a new selection set 
"
  )

(defun ssdel (ename ss)
  " Deletes an object (entity) from a selection set 
"
  )

(defun ssget (&optional mode pt1 pt2 pt-list filter-list)
  " Prompts the user to select objects (entities), and returns a selection set 
"
  )

(defun ssgetfirst ()
  " Determines which objects are selected and gripped 
"
  )

(defun sslength (ss)
    " Returns an integer containing the number of objects (entities) in a selection set 
"
  )

(defun ssmemb (ename ss)
  " Tests whether an object (entity) is a member of a selection set 
"
  )

(defun ssname (ss index)
  " Returns the object (entity) name of the indexed element of a selection set 
"
  )

(defun ssnamex (ss index)
  " Retrieves information about how a selection set was created 
"
  )

(defun sssetfirst (gripset &optional pickset)
  " Sets which objects are selected and gripped 
"
  )

"
vlax-curve-getArea
vlax-curve-getClosestPointTo
vlax-curve-getClosestPointToProjection
vlax-curve-getDistAtParam
vlax-curve-getDistAtPoint
vlax-curve-getEndParam
vlax-curve-getEndPoint
vlax-curve-getFirstDeriv
vlax-curve-getParamAtDist
vlax-curve-getParamAtPoint
vlax-curve-getPointAtDist
vlax-curve-getPointAtParam
vlax-curve-getSecondDeriv
vlax-curve-getStartParam
vlax-curve-getStartPoint
vlax-curve-isClosed
vlax-curve-isPeriodic
vlax-curve-isPlanar
"

