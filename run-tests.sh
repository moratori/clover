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


dot="`/usr/bin/which dot`"
if [ ${?} -ne 0 ]; then
        echo "Graphviz required for rendering refutation tree"
        exit 1
fi

dot_src_path="test-output-files/"

if [ -d "${dot_src_path}" ]; then
        find $dot_src_path -type f -name "*.dot" -exec dot -Tpng -o {}.png {} \;
fi
