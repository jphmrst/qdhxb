import Test.TLT
-- import Control.Monad.IO.Class
import Shiporder.Shiporder0 (testShiporder0)
import Shiporder.Shiporder1 (testShiporder1)
import Shiporder.Shiporder2 (testShiporder2)
import Books.Books1 (testBooks1)
import Books.BooksMod1 (testBooksMod1)
import Books.BooksMod2 (testBooksMod2)
import CustomersOrders.CustomersOrders1 (testCustomersOrders1)
import Age.Age (testAge)
import Age.Age0 (testAge0)
import Initials.Initials (testInitials)
import SizesNoList.SizesNoList (testSizesNoList)
import SizesAndList.SizesAndList (testSizesAndList)
import SizesAndList.SizesAndListInline (testSizesAndListInline)
import PersonAttr1.PersonAttr1 (testPersonAttr1)
import PersonAttr2.PersonAttr2 (testPersonAttr2)
import PersonAttr2.PersonAttr2Inline (testPersonAttr2Inline)
import PersonAttr2r.PersonAttr2r (testPersonAttr2r)
import GroupBasic.GroupBasic0 (testGB0)
import GroupBasic.GroupBasic1 (testGB1)
import ChoiceBasic.ChoiceBasic0 (testCB0)

import OFX1 (testOFX1)
import XSD1 (testXSD1)

main :: IO ()
main = do
  tlt $ do
    testAge
    testAge0
    testShiporder0
    testShiporder1
    testShiporder2
    testBooks1
    testBooksMod1
    testBooksMod2
    testCustomersOrders1
    testOFX1
    testXSD1
    testInitials
    testSizesNoList
    testSizesAndList
    testSizesAndListInline
    testPersonAttr1
    testPersonAttr2
    testPersonAttr2Inline
    testPersonAttr2r
    testGB0
    testGB1
    testCB0
