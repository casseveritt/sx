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
// constfold.h
//

#if !defined(__CONSTFOLD_H)
#define __CONSTFOLD_H 1

struct CgContext;

Expr *FoldConstants( CgContext *cg, Expr *fexpr);
Expr *ConstantFoldNode( CgContext *cg, Expr *fexpr, void *, int);

// struct operations defines the set of possible basic operations we might
// want to do on some data type
struct Operations {
  opcode      const_opcode;   // opcode to use for constants of this type
  void (*op_neg)(ScalarConstant *, const ScalarConstant *);
  void (*op_not)(ScalarConstant *, const ScalarConstant *);
  void (*op_bnot)(ScalarConstant *, const ScalarConstant *);
  void (*op_add)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_sub)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_mul)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_div)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_mod)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_and)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_or)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_xor)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_band)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_bor)(ScalarConstant *, const ScalarConstant *, const ScalarConstant *);
  void (*op_shr)(ScalarConstant *, const ScalarConstant *, int);
  void (*op_shl)(ScalarConstant *, const ScalarConstant *, int);
  int (*op_lt)(const ScalarConstant *, const ScalarConstant *);
  int (*op_gt)(const ScalarConstant *, const ScalarConstant *);
  int (*op_le)(const ScalarConstant *, const ScalarConstant *);
  int (*op_ge)(const ScalarConstant *, const ScalarConstant *);
  int (*op_eq)(const ScalarConstant *, const ScalarConstant *);
  int (*op_ne)(const ScalarConstant *, const ScalarConstant *);
  void (*cvtTo[TB_LastUser+1])(ScalarConstant *, const ScalarConstant *);
  void (*cvtFrom[TB_LastUser+1])(ScalarConstant *, const ScalarConstant *);
};

void Hal_SetupHalfFixedTypes(int, int);

#endif // !defined(__CONSTFOLD_H)

