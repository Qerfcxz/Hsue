{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Type
import qualified Data.Sequence as DS

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a)
do_request _=return