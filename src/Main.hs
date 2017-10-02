import Unfolding
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
    contents <- getContents
    let unfolding = parseUnfolding contents
    putStrLn $ "\nUnfolding stats:"
    putStrLn $ "  # events     = " ++ show (Set.size $ events unfolding)
    putStrLn $ "  # conditions = " ++ show (Set.size $ conditions unfolding)
    putStrLn $ "  # arcs       = " ++ show (arcs unfolding)

    putStrLn $ "\nLES stats:"
    putStrLn $ "  # events           = " ++ show (Set.size $ events unfolding)
    putStrLn $ "  # direct causality = " ++ show (dependencies unfolding)
    putStrLn $ "  # direct conflicts = " ++ show (conflictPairs unfolding)

    putStrLn $ "\nCPOG stats:"
    putStrLn $ "  # vertices                 = " ++ show (Set.size $ labels unfolding)
    putStrLn $ "  # arcs                     = " ++ show (labelDependencies unfolding)
    putStrLn $ "  # literals in predicates   = " ++ show (Set.size $ events unfolding)
    putStrLn $ "  # literals in rho function = "
             ++ show (2 * conflicts unfolding
                      + dependencies unfolding
                      + toInteger (Set.size $ events unfolding))
