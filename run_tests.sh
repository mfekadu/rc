#./testing/bi./testing/bash

echo -e "\nRunning assign-homes tests..."
./testing/assign-homes-tests.rkt

echo -e "\nRunning ec-tests..."
./testing/ec-tests.rkt

echo -e "\nRunning patch instructions tests..."
./testing/patch-instructions-tests.rkt

echo -e "\nRunning print-x86 tests..."
./testing/print-x86-tests.rkt

echo -e "\nRunning rco tests..."
./testing/rco-tests.rkt

echo -e "\nRunning select instructions tests..."
./testing/select-instructions-tests.rkt

echo -e "\nRunning uncover locals tests..."
./testing/uncover-locals-tests.rkt

echo -e "\nRunning uncover-live tests..."
./testing/uncover-live-tests.rkt

echo -e "\nRunning uniquify tests..."
./testing/uniquify-tests.rkt

echo -e "\nRunning graph tests..."
./testing/graph-tests.rkt

echo -e "\nRunning make-interference tests"
./testing/make-interference-tests.rkt

echo -e "\nRunning allocate-registers tests"
./testing/allocate-registers-tests.rkt

echo -e "\nRunning typecheck tests"
./testing/typecheck-tests.rkt

echo -e "\nRunning shrink tests"
./testing/shrink-tests.rkt
