'''
RLP library in Python

Writing my own library because I need support for partial parsing and re-entry
calls.

Reference: https://zxq9.com/archives/2749
'''

##############################################################################
# IMPORTS
##############################################################################

from math   import ceil
from typing import Union, List



##############################################################################
# TYPES
##############################################################################

'''
'RLPData' has to be in quotes because Python is retarded and doesn't
understand recursion.

See: https://stackoverflow.com/questions/53845024/defining-a-recursive-type-hint-in-python
'''
RLPData = Union[bytes, List['RLPData']]



##############################################################################
# API FUNCTIONS
##############################################################################

def encode(data: RLPData) -> bytes:
    '''
    Encode RLP data into bytes
    '''
    # RLP data is either bytes or a list
    # split on the two
    dt = type(data)
    if dt is bytes:
        return encode_bytes(data)
    elif dt is list:
        return encode_list(data)
    else:
        return ValueError('data must be list or bytes, instead is: {}; data: {}'.format(dt, data))



##############################################################################
# INTERNAL FUNCTIONS
##############################################################################

def encode_bytes(bs: bytes) -> bytes:
    '''
    Encode a bytestring into RLP
    '''
    bytelen = len(bs)
    # if it is a single byte between 0..127
    # then the result is the byte itself
    # python and is lazy
    if (bytelen == 1) and (bs[0] <= 127):
        return bs
    # if the bytestring is 0..55 items long, the first byte is 128 + length,
    # the rest of the string is the string
    elif bytelen <= 55:
        first_byte       = 128 + bytelen
        first_byte_bytes = encode_unsigned(first_byte)
        # python is so stupid
        result           = bytes([]).join([first_byte_bytes, bs])
        return result
    # if the bytestring is more than 55 items long, the first byte is 183 +
    # bytelength_of_bytelength
    # followed by the byte length
    # followed by the bytes
    else:
        bytelen_BYTES                  = encode_unsigned(bytelen)
        bytelen_of_bytelen_bytes_INT   = len(bytelen_BYTES)
        bytelen_of_bytelen_bytes_BYTES = encode_unsigned(bytelen_of_bytelen_bytes_INT)
        result                         = bytes([]).join([bytelen_of_bytelen_bytes_BYTES,
                                                         bytelen_BYTES,
                                                         bs])
        return result



##############################################################################
# MISC FUNCTIONS
##############################################################################

def encode_unsigned(n: int) -> bytes:
    '''
    Encode an integer into bytes
    '''
    bytelen = byte_length(n)
    return n.to_bytes(bytelen, byteorder='big')



def byte_length(n: int) -> int:
    '''
    return the number of bytes necessary to represent a non-negative integer
    '''
    if n == 0:
        return 1
    elif n > 0:
        return ceil(n.bit_length() / 8)
    else:
        raise ValueError('n must be a non-negative integer')
