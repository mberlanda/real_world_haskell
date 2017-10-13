# 12 Barcode Recognition


### Barcode Digits (EAN-13)

Section      | Length | Description
-------------|--------|------------
Number system | 2 |The first two digits. This can either indicate the nationality of the manufacturer or describe one of a few other categories, such as ISBN (book identifier) numbers.
Manufacturer ID | 5 | The next five digits. These are assigned by a country’s numbering authority.
Product ID | 5 | The next five digits. These are assigned by the manufacturer. (Smaller manufacturers may have a longer manufacturer ID and shorter product ID, but they still add up to 10 digits.)
Check digit | 1 | The last digit. This allows a scanner to validate the digit string it scans.
