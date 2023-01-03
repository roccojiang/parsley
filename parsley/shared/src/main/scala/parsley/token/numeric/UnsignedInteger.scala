/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.numeric

import parsley.Parsley, Parsley.{attempt, pure, unit}
import parsley.character.oneOf
import parsley.combinator.optional
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.character.charLift
import parsley.token.descriptions.numeric.{BreakCharDesc, NumericDesc}
import parsley.token.errors.ErrorConfig

private [token] final class UnsignedInteger(desc: NumericDesc, err: ErrorConfig, generic: Generic) extends Integer(desc) {

    // labelless versions
    protected [numeric] override lazy val _decimal: Parsley[BigInt] = generic.plainDecimal(desc)
    protected [numeric] override lazy val _hexadecimal: Parsley[BigInt] = attempt('0' *> noZeroHexadecimal)
    protected [numeric] override lazy val _octal: Parsley[BigInt] = attempt('0' *> noZeroOctal)
    protected [numeric] override lazy val _binary: Parsley[BigInt] = attempt('0' *> noZeroBinary)
    protected [numeric] override lazy val _number: Parsley[BigInt] = {
        if (desc.decimalIntegersOnly) decimal
        else {
            def addHex(p: Parsley[BigInt]) = {
                if (desc.integerNumbersCanBeHexadecimal) noZeroHexadecimal <|> p
                else p
            }
            def addOct(p: Parsley[BigInt]) = {
                if (desc.integerNumbersCanBeOctal) noZeroOctal <|> p
                else p
            }
            def addBin(p: Parsley[BigInt]) = {
                if (desc.integerNumbersCanBeBinary) noZeroBinary <|> p
                else p
            }
            val zeroLead = '0'.label("digit") *> (addHex(addOct(addBin(decimal <|> pure(BigInt(0))))))
            attempt(zeroLead <|> decimal)
        }
    }

    override def decimal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerUnsignedDecimal)(_decimal)
    override def hexadecimal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerUnsignedHexadecimal)(_hexadecimal)
    override def octal: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerUnsignedOctal)(_octal)
    override def binary: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerUnsignedBinary)(_binary)
    override def number: Parsley[BigInt] = ErrorConfig.label(err.labelIntegerUnsignedNumber)(_number)

    private def when(b: Boolean, p: Parsley[_]): Parsley[_] = if (b) p else unit

    val leadingBreakChar = desc.literalBreakChar match {
        case BreakCharDesc.NoBreakChar => unit
        case BreakCharDesc.Supported(breakChar, allowedAfterNonDecimalPrefix) => when(allowedAfterNonDecimalPrefix, optional(breakChar))
    }

    private val noZeroHexadecimal =
        when(desc.hexadecimalLeads.nonEmpty, oneOf(desc.hexadecimalLeads)) *> leadingBreakChar *>
        ErrorConfig.label(err.labelIntegerHexadecimalEnd)(generic.plainHexadecimal(desc))
    private val noZeroOctal =
        when(desc.octalLeads.nonEmpty, oneOf(desc.octalLeads)) *> leadingBreakChar *>
        ErrorConfig.label(err.labelIntegerOctalEnd)(generic.plainOctal(desc))
    private val noZeroBinary =
        when(desc.binaryLeads.nonEmpty, oneOf(desc.binaryLeads)) *> leadingBreakChar *>
        ErrorConfig.label(err.labelIntegerBinaryEnd)(generic.plainBinary(desc))

    override protected [numeric] def bounded[T](number: Parsley[BigInt], bits: Bits, radix: Int, label: (ErrorConfig, Boolean) => Option[String])
                                               (implicit ev: CanHold[bits.self,T]): Parsley[T] = ErrorConfig.label(label(err, false)) {
        number.collectMsg(err.messageIntTooLarge(_, bits.upperUnsigned, radix)) {
            case x if x <= bits.upperUnsigned => ev.fromBigInt(x)
        }
    }
}
