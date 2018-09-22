#!/bin/bash


roswell="`/usr/bin/which ros`"
if [ ${?} -ne 0 ]; then
        echo "Roswell required: https://github.com/roswell/roswell"
        exit 1
fi


lisp_implementation="`ros -e '(princ (string-downcase (lisp-implementation-type)))'`"

if [ "${lisp_implementation}" = "sbcl" ]; then
        $roswell -s sb-cover \
                 -s clover-test  \
                 -e '(1am:run)' \
                 -e '(sb-cover:report (merge-pathnames #P"coverage/" (asdf:system-source-directory :clover)))'
else
        $roswell -s clover-test \
                 -e '(1am:run)'
fi
