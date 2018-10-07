#!/bin/bash

#### roswell が見当たらない場合は終了
roswell="`/usr/bin/which ros`"
if [ ${?} -ne 0 ]; then
        echo "Roswell required: https://github.com/roswell/roswell"
        exit 1
fi



#### 各種変数定義
lisp_implementation="`$roswell -e '(princ (string-downcase (lisp-implementation-type)))'`"
lisp_implementation_version="`$roswell -e '(princ (lisp-implementation-version))'`"

##   プロジェクトルートを取得する(sbclの場合はsb-coverを読ませたいので呼び分ける)
case "${lisp_implementation}" in 
        "sbcl" ) project_root="`$roswell -s sb-cover -e '(princ (asdf:system-source-directory :clover))'`" ;;
        *      ) project_root="`$roswell -e '(princ (asdf:system-source-directory :clover))'`" ;;
esac

if [ ! -d "${project_root}" ]; then
        echo "can't get project root properly : ${project_root}"
        exit 1
fi

project_root="${project_root%/}"
dot_src_path="${project_root}/test-output-files/graphviz/"
coverage_path="${project_root}/coverage/"



#### テスト実行結果等を出力するためのディレクトリを作成
/bin/mkdir -p "${dot_src_path}"
/bin/mkdir -p "${coverage_path}"



#### 処理系ごとにテスト実行(sbclの場合はcoverageを出力したいため呼び分ける)
case "${lisp_implementation}" in 
        "sbcl" ) $roswell -s sb-cover \
                   -s clover-test  \
                   -e '(1am:run)' \
                   -e '(sb-cover:report (merge-pathnames #P"coverage/" (asdf:system-source-directory :clover)))';;
        *      ) $roswell -s clover-test \
                   -e '(1am:run)'
esac



#### Graphvizが存在する場合は、描画まで実行する
dot="`/usr/bin/which dot`"
if [ ${?} -eq 0 ]; then
        if [ -d "${dot_src_path}" ]; then
                /usr/bin/find $dot_src_path -type f -name "*.dot" -exec $dot -Tpng -o {}.png {} \;
        fi
else
        echo "Graphviz required for rendering refutation tree"
        echo "test skipped"
        exit 1
fi
