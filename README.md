`task-scheduler`

![Haskell](https://img.shields.io/badge/language-Haskell-blue)
![License](https://img.shields.io/badge/license-MIT-green)

**Task Scheduler на Haskell** — библиотека для управления задачами с зависимостями (DAG) и параллельным выполнением.

Позволяет:

* Добавлять задачи с зависимостями
* Выполнять задачи параллельно
* Управлять статусами задач: Pending, Running, Finished, Cancelled
* Задавать таймауты и получать результаты выполнения

---

##  Структура проекта

```text
task-scheduler/
├─ src/
│  ├─ Task.hs       # Основная логика Task Pool
│  ├─ Lib.hs        # Дополнительные утилиты
│  └─ Paths_task.hs # Автогенерация Stack/Cabal
├─ test/
│  ├─ Tests.hs      # Тесты для Task Scheduler
│  └─ Spec.hs       # Спецификации
├─ package.yaml     # Настройки Stack / Cabal
├─ stack.yaml       # Stack конфигурация
└─ README.md
```

---

## ⚙️ Установка

Нужен **GHC 9.10.3** и **Stack**.

Клонируем репозиторий:

```bash
git clone https://github.com/ryulal01/task-scheduler.git
cd task-scheduler
```

Собираем библиотеку:

```bash
stack build
```

Запускаем тесты:

```bash
stack test
```

---

## Использование

Пример создания и запуска задач:

```haskell
import Task
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad (void)

main :: IO ()
main = do
    -- создаём пул с 4 потоками
    pool <- newPool 4

    -- простая задача
    let action1 = putStrLn "Task 1 running" >> threadDelay 1000000
    t1 <- submit action1 [] pool

    -- задача с зависимостью от t1
    let action2 = putStrLn "Task 2 running"
    t2 <- submit action2 [t1] pool

    -- ждём завершения всех задач
    wait pool
```

###  С таймаутом

```haskell
_ <- submitWithTimeout 500000 (threadDelay 1000000 >> putStrLn "Done") [] pool
```

---

##  Тесты

Используем `tasty` и `HUnit`.

Пример теста:

```haskell
testCase "single task runs" $ do
    pool <- newPool 4
    ref <- newTVarIO False
    _ <- submit (atomically $ writeTVar ref True) [] pool
    wait pool
    val <- readTVarIO ref
    assertBool "Task did not run" val
```

Запуск всех тестов:

```bash
stack test
```

---

##  Бенчмарк

Пример измерения производительности:

```haskell
import Criterion.Main
import Task

main = defaultMain [
    bgroup "task-pool" [
        bench "100 tasks" $ whnfIO $ do
            pool <- newPool 4
            mapM_ (\_ -> submit (return ()) [] pool) [1..100]
            wait pool
    ]
]
```

Сборка и запуск:

```bash
stack bench
```

---

## 🔧 Структура данных

* `Task` — задача
* `TaskId` — идентификатор задачи
* `Pool` — пул потоков
* Статусы: `Pending`, `Running`, `Finished`, `Cancelled`

> Внутри используется `TVar` для хранения состояния и `Map TaskId Task` для быстрого доступа.

---

## Улучшения

* Ограничение параллельности через `QSem`
* Таймауты задач
* Хранение результатов (`Either Exception ()`)
* Эффективная структура данных (`Map`)
* Тесты и бенчмарки для проверки производительности


# task7
