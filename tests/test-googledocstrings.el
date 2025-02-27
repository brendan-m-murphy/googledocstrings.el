;;; test-googledocstrings.el --- Testing for googledocstrings.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Doug Davis

;; Author: Doug Davis <ddavis@ddavis.io>
;; Keywords: testing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Testing code for googledocstrings.el

;;; Code:

(require 'buttercup)
(require 'googledocstrings)


(describe "Function signature parsing"
  :var ((fsig1 "\
def f(
    a: int, b: float = 5.5, c: str | int | None = None
) -> float:")
        (fsig2 "def f(x, y=5, z=None):")
        (fsig3 "\
def somelongerfunc(
    a1: np.ndarray,
    a2: np.ndarray | None = None,
    a3: Sequence[float | None] | None = None,
) -> tuple[int, float]:")
        (fsig4 "\
def f(
    aa,
    bb=(4, 6),
    cc: set = {1, 2},
    dd: dict[str, int] = dict(a=1, b=2, c = 3),
    ee: dict[str, int] = {\"a\": 5, \"b\": 6},
    ff={\"a\": 5, \"b\": 6},
    gg=\"str,str\",
    hh: str = \"str, str, str, str\",
    ii: tuple[int, ...] = (4, 6),
    jj: str = \"str,str\",
)")
        (fsig-ignored-args "\
def func_with_ignored_args(
    self,
    a1: int,
    a2: str,
    *args,
    **kwargs,
)"))
  (it "Checks arg parsing 1"
    (let ((a (make-googledocstrings--arg :name "a" :type "int" :defval nil))
          (b (make-googledocstrings--arg :name "b" :type "float" :defval "5.5"))
          (c (make-googledocstrings--arg :name "c"
                                 :type "str | int, optional"
                                 :defval "None"))
          (args (googledocstrings--def-args (googledocstrings--parse-def fsig1)))
          (ret (googledocstrings--def-rtype (googledocstrings--parse-def fsig1))))
      (expect a :to-equal (car args))
      (expect b :to-equal (nth 1 args))
      (expect c :to-equal (nth 2 args))
      (expect ret :to-equal "float")))

  (it "Checks arg parsing 2"
    (let ((x (make-googledocstrings--arg :name "x" :type nil :defval nil))
          (y (make-googledocstrings--arg :name "y" :type nil :defval "5"))
          (z (make-googledocstrings--arg :name "z" :type nil :defval "None"))
          (args (googledocstrings--def-args (googledocstrings--parse-def fsig2)))
          (ret (googledocstrings--def-rtype (googledocstrings--parse-def fsig2))))
      (expect x :to-equal (car args))
      (expect y :to-equal (nth 1 args))
      (expect z :to-equal (nth 2 args))
      (expect nil :to-be ret)))

  (it "Checks arg parsing 3"
    (let ((a1 (make-googledocstrings--arg :name "a1"
                                  :type "np.ndarray"
                                  :defval nil))
          (a2 (make-googledocstrings--arg :name "a2"
                                  :type "np.ndarray, optional"
                                  :defval "None"))
          (a3 (make-googledocstrings--arg :name "a3"
                                  :type "Sequence[float | None], optional"
                                  :defval "None"))
          (args (googledocstrings--def-args (googledocstrings--parse-def fsig3)))
          (ret (googledocstrings--def-rtype (googledocstrings--parse-def fsig3))))
      (expect a1 :to-equal (car args))
      (expect a2 :to-equal (nth 1 args))
      (expect a3 :to-equal (nth 2 args))
      (expect ret :to-equal "tuple[int, float]")))

  (it "Checks arg parsing 4"
      (let ((aa (make-googledocstrings--arg :name "aa"
                                    :type nil
                                    :defval nil))
            (bb (make-googledocstrings--arg :name "bb"
                                    :type nil
                                    :defval "(4, 6)"))
            (cc (make-googledocstrings--arg :name "cc"
                                    :type "set"
                                    :defval "{1, 2}"))
            (dd (make-googledocstrings--arg :name "dd"
                                    :type "dict[str, int]"
                                    :defval "dict(a=1, b=2, c = 3)"))
            (ee (make-googledocstrings--arg :name "ee"
                                    :type "dict[str, int]"
                                    :defval "{\"a\": 5, \"b\": 6}"))
            (ff (make-googledocstrings--arg :name "ff"
                                    :type nil
                                    :defval "{\"a\": 5, \"b\": 6}"))
            (gg (make-googledocstrings--arg :name "gg"
                                    :type nil
                                    :defval "\"str,str\""))
            (hh (make-googledocstrings--arg :name "hh"
                                    :type "str"
                                    :defval "\"str, str, str, str\""))
            (ii (make-googledocstrings--arg :name "ii"
                                    :type "tuple[int, ...]"
                                    :defval "(4, 6)"))
            (jj (make-googledocstrings--arg :name "jj"
                                    :type "str"
                                    :defval "\"str,str\""))
            (args (googledocstrings--def-args (googledocstrings--parse-def fsig4))))

        (expect aa :to-equal (car args))
        (expect bb :to-equal (nth 1 args))
        (expect cc :to-equal (nth 2 args))
        (expect dd :to-equal (nth 3 args))
        (expect ee :to-equal (nth 4 args))
        (expect ff :to-equal (nth 5 args))
        (expect gg :to-equal (nth 6 args))
        (expect hh :to-equal (nth 7 args))
        (expect ii :to-equal (nth 8 args))
        (expect jj :to-equal (nth 9 args))))
  (it "Checks arg parsing for ignored param names"
    (let* ((googledocstrings-ignored-params (list "self" "*args" "**kwargs"))
           (self (make-googledocstrings--arg :name "self"
                                     :type nil
                                     :defval nil))
           (a1 (make-googledocstrings--arg :name "a1"
                                   :type "int"
                                   :defval nil))
           (a2 (make-googledocstrings--arg :name "a2"
                                   :type "str"
                                   :defval nil))
           (pyargs (make-googledocstrings--arg :name "*args"
                                       :type nil
                                       :defval nil))
           (kwargs (make-googledocstrings--arg :name "**kwargs"
                                       :type nil
                                       :defval nil))

          (args (googledocstrings--def-args (googledocstrings--parse-def fsig-ignored-args))))
      (expect a1 :to-equal (car args))
      (expect a2 :to-equal (nth 1 args))
      (expect args :not :to-contain self)
      (expect args :not :to-contain pyargs)
      (expect args :not :to-contain kwargs))))

(provide 'test-googledocstrings)
;;; test-googledocstrings.el ends here
