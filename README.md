<b>ФПрог, лаб 1</b>

Выполнил Евтухович Виталий (151005)

<b>Применение:</b>
```
Usage: <interactive> FILE [-r|--razdel CHAR] [-q|--first_line BOOL]
                     [-f|--first_column BOOL] [-l|--last_column BOOL]
                     [-c|--clusters INT] [-e|--dvtn DOUBLE]
                     [-d|--distance_func NAME] [-i|--init_method NAME]
                     [-o|--output FILE]

Available options:
  -h,--help                Show this help text
  FILE                     Input file
  -r,--razdel CHAR         Razdelitel in CSV file (defualt ',')
  -q,--first_line BOOL     Condition to use first line (default true)
  -f,--first_column BOOL   Condition to use first column (default true)
  -l,--last_column BOOL    Condition to use last column (default false)
  -c,--clusters INT        Number of clusters (default 2)
  -e,--dvtn DOUBLE         Tochnost' (default 0.001)
  -d,--distance_func NAME  Method of distance cuclulating: "Haming" or
                           "Evclid"(default "Evclid")
  -i,--init_method NAME    Method of inirialization: "Random_matrix" or
                           "Random_centers" (default "Random_centers")
  -o,--output FILE         Output file
  ```
  
  <b>Запуск программы (пример):</b>
  ```
  :main "butterfly.txt"
  ```
