import Test.TLT
-- import Control.Monad.IO.Class
import Shiporder0 (testShiporder0)
import Shiporder1 (testShiporder1)
import Shiporder2 (testShiporder2)
import Books.Books1 (testBooks1)
import Books.BooksMod1 (testBooksMod1)
import Books.BooksMod2 (testBooksMod2)
import CustomersOrders.CustomersOrders1 (testCustomersOrders1)
import OFX1 (testOFX1)
import XSD1 (testXSD1)
import Age.Age (testAge)
import Initials (testInitials)
import SizesNoList.SizesNoList (testSizesNoList)
import SizesAndList (testSizesAndList)
import SizesAndListInline (testSizesAndListInline)
import PersonAttr1.PersonAttr1 (testPersonAttr1)
import PersonAttr2.PersonAttr2 (testPersonAttr2)
import PersonAttr2r.PersonAttr2r (testPersonAttr2r)

main :: IO ()
main = do
  tlt $ do
    testShiporder0
    testShiporder1
    testShiporder2
    testBooks1
    testBooksMod1
    testBooksMod2
    testCustomersOrders1
    testOFX1
    testXSD1
    testAge
    testInitials
    testSizesNoList
    testSizesAndList
    testSizesAndListInline
    testPersonAttr1
    testPersonAttr2
    testPersonAttr2r
