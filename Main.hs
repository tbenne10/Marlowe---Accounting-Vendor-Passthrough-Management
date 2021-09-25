{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module VendorPassthrough where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False


vendor, lessor, lessee :: Party
vendor = Role "Vendor"
lessor = Role "Lessor"
lessee = Role "Lessee"

servicePrice, percentFee, fixedFee, lessorRevenue, passthroughRevenue :: Value
servicePrice = ConstantParam "Service Price"
percentFee = ConstantParam "Percent Fee"
fixedFee = ConstantParam "Fixed Fee"
lessorRevenue =  ConstantParam "lessor revenue" --Constant fixedFee + ((percentFee/100) * servicePrice)
passthroughRevenue =  ConstantParam "passthrough revenue"--Constant servicePrice - lessorRevenue 

--Ignore - testing purposes
--testLessorRevenue, ex :: Value
--ex = ConstantParam "0.01"
--testLessorRevenue = AddValue fixedFee (MulValue (MulValue percentFee ex) servicePrice)

lesseeRecievablesTimeout, vendorPayablesTimeout, answerTimeout :: Timeout
lesseeRecievablesTimeout = SlotParam "Lessee A/R timeout" -- If money not recieved, front the funds to the vendor. 
vendorRecievablesTimeout = SlotParam "Vendor -> Lessor A/R timeout" --If vendor does not pay, front funds. 
lesseePayablesTimeout = SlotParam "Lessor -> Lessee A/P timeout" --If lessee is not paid a refund, force payment. 
vendorPayablesTimeout = SlotParam "Lessor -> Vendor A/P timeout" --If vendor is not paid, force payment 
answerTimeout = SlotParam "Response timeout"  

choice :: ChoiceName -> Party -> Integer -> Contract -> Case
choice choiceName chooser choiceValue = Case (Choice (ChoiceId choiceName chooser)
                                                     [Bound choiceValue choiceValue])


choices :: Timeout -> Party -> Contract -> [(Integer, ChoiceName, Contract)] -> Contract
choices timeout chooser timeoutContinuation list =
    When [choice choiceName chooser choiceValue continuation
          | (choiceValue, choiceName, continuation) <- list]
         timeout
         timeoutContinuation


refundLessor :: Contract
refundLessor
 | explicitRefunds = Pay lessor (Party lessor) ada servicePrice Close
 | otherwise = Close

refundLessee :: Contract
refundLessee
 | explicitRefunds = Pay lessee (Party lessee) ada servicePrice Close
 | otherwise = Close

frontVendorFunds :: Contract
frontVendorFunds
 | explicitRefunds = Pay vendor (Party lessor) ada servicePrice Close
 | otherwise = Close


transferToLessee :: Timeout -> Value -> Contract -> Contract
transferToLessee  timeout amount continuation =
    When [ Case (Deposit lessee lessor ada amount) continuation ]
         timeout
         Close

transferFromLessee :: Timeout -> Value -> Contract -> Contract
transferFromLessee  timeout amount continuation =
    When [ Case (Deposit lessor lessee ada amount) continuation ]
         timeout
         Close

transferFromVendor :: Timeout -> Value -> Contract -> Contract
transferFromVendor timeout amount continuation =
    When [ Case (Deposit lessor vendor ada amount) continuation ]
         timeout
         Close

transferToVendor :: Timeout -> Value -> Contract -> Contract
transferToVendor timeout amount continuation =
    When [ Case (Deposit vendor lessor ada amount) continuation ]
         timeout
         Close


contract :: Contract 
contract = 
           choices lesseeRecievablesTimeout vendor Close
              [ (0, "Vendor performs service without issue"
                , transferFromLessee lesseeRecievablesTimeout servicePrice 
                $ transferToVendor vendorPayablesTimeout passthroughRevenue
                Close
                )
                , (1, "Vendor performs service with dispute from lessee"
                ,  choices answerTimeout lessee frontVendorFunds
                     [ (1, "Refund requested"
                       , transferToLessee lesseePayablesTimeout servicePrice
                       $ transferFromVendor vendorRecievablesTimeout servicePrice
                       Close
                       )
                     , (0, "Dispute terminated"
                        , transferFromLessee lesseeRecievablesTimeout servicePrice 
                        $ transferToVendor vendorPayablesTimeout passthroughRevenue
                        Close
                        
                     
                       )
                     ]
                )
              ]




