#!/bin/bash

#### 使用コマンド有無確認
roswell="`/usr/bin/which ros`"
timeout="`/usr/bin/which timeout`"
dot="`/usr/bin/which dot`"
if [ -z "$roswell" ] || [ -z "$timeout" ]; then
        echo "following command required"
        echo "* ros"
        echo "* timeout"
        exit 1
fi



#### 各種変数定義
lisp_implementation="`$roswell -e '(princ (string-downcase (lisp-implementation-type)))'`"
lisp_implementation_version="`$roswell -e '(princ (lisp-implementation-version))'`"
test_duration_time=180

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
dot_src_path="${project_root}/tests/test-output-files/graphviz/"
coverage_path="${project_root}/coverage/"



#### テスト実行結果等を出力するためのディレクトリを作成
if [ "${project_root}" != "" ];then
      /usr/bin/rm -r "${coverage_path}"
fi
/bin/mkdir -p "${dot_src_path}"
/bin/mkdir -p "${coverage_path}"



#### 処理系ごとにテスト実行(sbclの場合はcoverageを出力したいため呼び分ける)
test_result=0
case "${lisp_implementation}" in 
        "sbcl" ) $timeout -k 3 $test_duration_time \
                   $roswell -s sb-cover \
                            -s clover-test \
                            -e '(sb-sprof:start-profiling :mode :cpu)' \
                            -e '(1am:run)' \
                            -e '(sb-sprof:stop-profiling) ' \
                            -e '(sleep 2)' \
                            -e '(sb-cover:report (merge-pathnames #P"coverage/" (asdf:system-source-directory :clover)) :if-matches (lambda (f) (search "clover/src/" f)))' \
                            -e '(sb-sprof:report :type :flat :min-percent 3 :sort-order :descending)'
                 test_result=$?;;
        *      ) $timeout -k 3 $test_duration_time \
                   $roswell -s clover-test \
                            -e '(1am:run)'
                 test_result=$?
esac

if [ $test_result -ne 0 ]; then
        echo ""
        echo "test execution timeouted"
        echo "test aborted"
        exit $test_result
fi


#### パス名などの情報をcoverageファイルから削除
if [ -d "${coverage_path}" ]; then
      find "${coverage_path}" -type f -name '*.html' | xargs -I{} sh -c "sed -i -e \"s!${project_root}!!g\" {}"
fi


#### Graphvizが存在する場合は、描画まで実行する
if [ ! -z "${dot}" ]; then
        if [ -d "${dot_src_path}" ]; then
                /usr/bin/find $dot_src_path -type f -name "*.dot" -exec $dot -Tpng -o {}.png {} \;
        fi
else
        echo "Graphviz required for rendering refutation tree"
        echo "test skipped"
fi

exit 0
