module BotRead (
    botMessage ) where

botMessage :: Show a => a -> IO ()
botMessage message =
    putStrLn "" >>
    putStrLn "                ▄▄█▀▀██▄▄       " >>
    putStrLn "             ▄█▀▀░░░░░░░▀█      " >>
    putStrLn "           ▄▀░░░░░░░░░░░░░█     " >>
    putStrLn "         ▄█░░░░░░░░░░░░░░░█     " >>
    putStrLn "       ██▀░░░░░░░▄▄▄░░▄░█▄█▄    " >>
    putStrLn "     ▄▀░░░░░░░░░░████░█▄██░▀▄   " >>
    putStrLn "    █▀░░░░░░░░▄▄██▀  █████░██   " >>
    putStrLn "   █▀░░░░░░░░░▀█ ▀█▀█▀▀▄██▄█▀   " >>
    putStrLn "   ██░░░░░░░░░░█  █ █  ▀▀▄█▀    " >>
    putStrLn "   █░░░░░█░░░▀█    ▄     ▄█     " >>
    putStrLn "    ▀█░░░░███▄░█      ▄▄▄▄█▀█▄  " >>
    putStrLn "     ▀██░░█▄▀▀██        ▄▄█  ▀▄ " >>
    putStrLn "      ▀▀█▄░▀▄▄ ▄       ███▀  ▄██" >>
    putStrLn "         ▀▀▀███▀█▄     █▀     ▀█" >>
    putStrLn "            ▄▀   ▀█▄         ▄█▀" >>
    putStrLn "   ▄▄▄▀▀▀▀▀█▀▀█   █▄▀▄▄▄▄▄▄█▀▀ |\\" >>
    putStrLn " ▄█░░░▄██▀░░░░▀█████▄          | \\" >>
    putStrLn "█▀▀░▄█░░░░░░░░░░░░░░▀▀█▄       |  \\" >>
    putStrLn "█░░░█░░░░░░░░░░░░░░░░░░█▄      |   \\" >>
    putStrLn "                               |    \\" >>
    putStrLn "______________________________/      \\__________" >>
    putStrLn "" >>
    putStrLn (" " ++ show message) >>
    putStrLn "_________________________________________________" >>
    putStrLn ""
