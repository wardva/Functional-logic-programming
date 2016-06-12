import Control.Concurrent.STM

type Account = TVar Int

widthdraw :: Account -> Int -> STM ()
widthdraw acc amount = do
  bal <- readTVar acc
  writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = widthdraw acc (-amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount = atomically $ deposit to amount >>
                                       widthdraw from amount

limitedWithdraw acc amount = do bal <- readTVar acc
                                if amount > 0 && amount > bal
                                  then retry
                                  else writeTVar acc (bal - amount)

limitedWithdraw' acc amount = do bal <- readTVar acc
                                 check $ amount > 0 && amount > bal
                                 writeTVar acc (bal - amount)


main :: IO ()
main = undefined
