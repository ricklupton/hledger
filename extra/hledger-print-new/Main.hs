#!/usr/bin/env runhaskell
{-|
hledger-print-new [-f JOURNALFILE | -f-] <new journal files...>

Print only journal entries from the new files which do not appear in
the existing journal file. Reads the default or specified journal, or
stdin.

|-}

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.List
import Data.Ord
import Data.Data
import Hledger.Read (readJournalFiles)
import Hledger.Cli
import Hledger.Cli.Print
import Hledger.Query
import Hledger.Data.Dates
import Hledger.Data.RawOptions (listofstringopt)
import Hledger.Utils.Regex

argsmode :: Mode RawOpts
argsmode = (defCommandMode ["hledger-print-new"])
           { modeHelp = "print a journal entry posting the total balance of all accounts"
                        ++ " (or the specified account and its subaccounts)"
           , modeArgs = ([], Just $ argsFlag "[FILES]")
           , modeGroupFlags = Group
                              { groupNamed =
                               [ ("Input", inputflags)
                               , ("Output", outputflags)
                                 -- , ("Reporting",reportflags)
                               , ("Misc",helpflags)
                               ]
                              , groupUnnamed = []
                              , groupHidden = []
                              }
           }

main :: IO ()
main = do
  opts <- getCliOpts argsmode
  withJournalDo opts (process' printNew)

process' :: (CliOpts -> Journal -> Journal -> IO ()) -> CliOpts -> Journal -> IO ()
process' cmd opts j = do
  let newFiles = listofstringopt "args" $ rawopts_ opts
  rulespath <- rulesFilePathFromOpts opts
  ej <- readJournalFiles Nothing rulespath False newFiles
  either error' (cmd opts j . journalApplyAliases (aliasesFromOpts opts)) ej

-- exclude transactions which match by: date, desc (start of), amount, account, currency
-- match by 'reg'

printNew :: CliOpts -> Journal -> Journal -> IO ()
printNew _ jold Journal{jtxns=txns} = do
  putStr $ concatMap showTransactionUnelided $
    filter (not . transactionExists jold) txns

transactionExists :: Journal -> Transaction -> Bool
transactionExists j Transaction{tdate=day,tdescription=desc,tcode=code,tpostings=_:p2:_} = 
  any (q `matchesPosting`) (journalPostings j)
  where
    q = And [ Acct acct
            , Date date
            , Amt Eq (aquantity amt)
            , Sym (acommodity amt)
            , optionalMatch Code True code
            , optionalMatch Desc False desc
            ]
    acct = paccount $ p2
    date = DateSpan (Just day) (Just $ addDays 1 day)
    amt  = head $ amounts $ pamount p2

optionalMatch :: (Regexp -> Query) -> Bool -> String -> Query
optionalMatch queryType matchEnd = matcher
  where
    matcher "" = Any
    matcher x  = queryType $ case matchEnd of
                               True  -> "^" ++ (escape x) ++ "$"
                               False -> "^" ++ (escape x)

escape :: String -> String
escape s = concatMap escapeChar s
  where
    escapeChar c | c `elem` regexChars = '\\' : [c]
                 | otherwise = [c]
    regexChars = "*\\+()^$.{}]|"
