;;;; src/geometry/svg/package.lisp

(defpackage #:vlisp/geometry-svg
  (:use #:cl)
  (:import-from #:vlisp/geometry
   #:<geometric-object>
   #:<point-2d> #:x #:y
   #:<line-2d> #:<line-2d>-start-point #:<line-2d>-end-point
   #:<circle-2d> #:<circle-2d>-center #:<circle-2d>-radius
   #:<arc-2d> #:<arc-2d>-center #:<arc-2d>-radius #:<arc-2d>-start-angle #:<arc-2d>-end-angle)
  (:export
   #:render-to-svg
   #:write-svg-file))

(in-package #:vlisp/geometry-svg)
