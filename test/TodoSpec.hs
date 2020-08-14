module TodoSpec (spec) where

import Data.Time (fromGregorian)
import Test.Hspec
import Test.QuickCheck (Testable (property))
import Todo

spec = do
  describe "Parsing according to todotxt.org" $ do
    context "when just a description is set" $ do
      it "parses and is an undone description" $
        let sut = read "Review Tim's pull request" :: Todo
         in do
              sut `shouldSatisfy` (not . isDone)
              getDescription sut `shouldBe` "Review Tim's pull request"

      it "parses and is an   done description" $
        let sut = read "x Review Tim's pull request" :: Todo
         in do
              sut `shouldSatisfy` isDone
              getDescription sut `shouldBe` "Review Tim's pull request"

      it "is only description despite trailing x" $
        let sut = read "xReview Tim's pull request" :: Todo
         in do
              sut `shouldSatisfy` (not . isDone)
              getDescription sut `shouldBe` "xReview Tim's pull request"

    context "with creation/completion date" $ do
      it "has a creation date set" $
        let sut = read "2011-03-01  Review Tim's pull request"
            expectedCreationDate = Just $ fromGregorian 2011 03 01
         in do
              sut `shouldSatisfy` (not . isDone)
              getCompletionDate sut `shouldBe` Nothing
              getCreationDate sut `shouldBe` expectedCreationDate
              getDescription sut `shouldBe` "Review Tim's pull request"
              getPriority sut `shouldBe` Nothing
      it "has a creation date set and is done" $
        let sut = read "x 2011-03-01  Review Tim's pull request"
            expectedCreationDate = Just $ fromGregorian 2011 03 01
         in do
              sut `shouldSatisfy` isDone
              getCompletionDate sut `shouldBe` Nothing
              getCreationDate sut `shouldBe` expectedCreationDate
              getDescription sut `shouldBe` "Review Tim's pull request"
              getPriority sut `shouldBe` Nothing
      it "has dates and description" $
        let sut = read "2020-05-08 2018-01-18 foo"
         in do
              getCompletionDate sut `shouldBe` Just (fromGregorian 2020 05 08)
              getCreationDate sut `shouldBe` Just (fromGregorian 2018 01 18)
              getDescription sut `shouldBe` "foo"
      it "is done, has dates and description" $
        let sut = read "x 2020-05-08 2018-01-18 foo"
         in do
              sut `shouldSatisfy` isDone
              getCompletionDate sut `shouldBe` Just (fromGregorian 2020 05 08)
              getCreationDate sut `shouldBe` Just (fromGregorian 2018 01 18)
              getDescription sut `shouldBe` "foo"

    context "when all fields are set but NO tags" $ do
      it "parses succesfully" $
        let sut = read "x (A) 2011-03-02 2011-03-01 Review Tim's pull request" :: Todo
            expectedCompletionDate = Just $ fromGregorian 2011 03 02
            expectedCreationDate = Just $ fromGregorian 2011 03 01
         in do
              sut `shouldSatisfy` isDone
              getCompletionDate sut `shouldBe` expectedCompletionDate
              getCreationDate sut `shouldBe` expectedCreationDate
              getDescription sut `shouldBe` "Review Tim's pull request"
              getPriority sut `shouldBe` Just "A"

  describe "Serialize according to todotxt.org" $ do
    it "simple reverseibility todo == (read.show) todo " $
      let expected = read "Review Tim's pull request" :: Todo
       in do
            (read . show) expected `shouldBe` expected
    it "read is inverse to show" $
      property $
        \todo -> (read . show) todo == (todo :: Todo)
