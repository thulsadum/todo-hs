module TodoSpec (spec) where

import Test.Hspec

import Todo

spec = do
    describe "Parsing according to todotxt.org" $ do
        context "when all fields are set (completed, priority \
                \ completion date, creation date, description, but NO tags" $ do
            it "parses succesfully" $
                let sut = read "x 2011-03-02 2011-03-01 Review Tim's pull request" :: Todo
                in pending