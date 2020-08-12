module TodoSpec (spec) where

import Test.Hspec

import Todo

spec = do
    describe "Parsing according to todotxt.org" $ do
        context "when just a description is set" $ do
            it "parses and is an undone description" $
                let
                    sut = read "Review Tim's pull request" :: Todo
                in do
                    sut `shouldSatisfy` (not.isDone)
                    getDescription sut `shouldBe` "Review Tim's pull request"

            it "parses and is an   done description" $
                let
                    sut = read "x Review Tim's pull request" :: Todo
                in do
                    sut `shouldSatisfy` isDone
                    getDescription sut `shouldBe` "Review Tim's pull request"

            it "is only description despite trailing x" $
                let
                    sut = read "xReview Tim's pull request" :: Todo
                in do
                    sut `shouldSatisfy` (not.isDone)
                    getDescription sut `shouldBe` "xReview Tim's pull request"

        context "with creation/deletiong date" $ do
            it "..."
                pending

        context "when all fields are set but NO tags" $ do
            it "parses succesfully" $
                let sut = read "x (A) 2011-03-02 2011-03-01 Review Tim's pull request" :: Todo
                in do
                    sut `shouldSatisfy` isDone
                    -- getCompletionDate sut `shouldBe` ...
                    -- getCreationDate sut
                    -- getDescription
                    -- getPriority