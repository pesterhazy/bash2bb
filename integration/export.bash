VAR=a; export VAR; python -c "import os; print(os.getenv('VAR'))"
