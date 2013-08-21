/****************************************************************************\
Copyright (c) 2002, NVIDIA Corporation.

NVIDIA Corporation("NVIDIA") supplies this software to you in
consideration of your agreement to the following terms, and your use,
installation, modification or redistribution of this NVIDIA software
constitutes acceptance of these terms.  If you do not agree with these
terms, please do not use, install, modify or redistribute this NVIDIA
software.

In consideration of your agreement to abide by the following terms, and
subject to these terms, NVIDIA grants you a personal, non-exclusive
license, under NVIDIA's copyrights in this original NVIDIA software (the
"NVIDIA Software"), to use, reproduce, modify and redistribute the
NVIDIA Software, with or without modifications, in source and/or Binary
forms; provided that if you redistribute the NVIDIA Software, you must
retain the copyright notice of NVIDIA, this notice and the following
text and disclaimers in all such redistributions of the NVIDIA Software.
Neither the name, trademarks, service marks nor logos of NVIDIA
Corporation may be used to endorse or promote products derived from the
NVIDIA Software without specific prior written permission from NVIDIA.
Except as expressly stated in this notice, no other rights or licenses
express or implied, are granted by NVIDIA herein, including but not
limited to any patent rights that may be infringed by your derivative
works or by other works in which the NVIDIA Software may be
incorporated. No hardware is licensed hereunder. 

THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION, WARRANTIES OR CONDITIONS OF TITLE,
NON-INFRINGEMENT, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
ITS USE AND OPERATION EITHER ALONE OR IN COMBINATION WITH OTHER
PRODUCTS.

IN NO EVENT SHalL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT,
INCIDENTAL, EXEMPLARY, CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, LOST PROFITS; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) OR ARISING IN ANY WAY
OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE
NVIDIA SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT,
TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF
NVIDIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
\****************************************************************************/

//
// type.h
//

#if ! __SX_TYPE_H__
#define __SX_TYPE_H__ 1

#define MAX_ARRAY_DIMENSIONS 3

// Type properties:

enum TypeBase {
	TB_NoType, TB_UndefinedType, TB_Cfloat, TB_Cint,
	TB_Void, TB_Float, TB_Int, TB_Boolean, TB_FirstUser = 0x8, TB_LastUser = 0xf
};

enum TypeCategory {
	TC_Invalid, TC_None, TC_Base, TC_Connector, TC_Scalar, TC_Array, TC_Struct, TC_Function
};

enum TypeDomain {
	TD_Invalid, TD_Unknown, TD_Uniform, TD_Varying
};

enum TypeQualifier {
	TQ_Invalid, TQ_None, TQ_In, TQ_Out, TQ_InOut
};

// ??? Should these be called "declarator bits"???
#define TYPE_MISC_MASK              0x0ff
#define TYPE_MISC_TYPEDEF           0x001
#define TYPE_MISC_PROGRAM           0x002    // Type is program function
#define TYPE_MISC_ABSTRACT_PARAMS   0x004    // Type is function declared with abstract parameters
#define TYPE_MISC_VOID              0x008    // Type is void
#define TYPE_MISC_INLINE            0x010    // "inline" function attribute
#define TYPE_MISC_INTERNAL          0x020    // "__internal" function attribute
#define TYPE_MISC_PACKED            0x040    // For vector types like float3
#define TYPE_MISC_PACKED_KW         0x080    // Actual "packed" keyword used
#define TYPE_MISC_MARKED            0x100    // Temp value for printing types, etc.

#endif // __SX_TYPE_H__

