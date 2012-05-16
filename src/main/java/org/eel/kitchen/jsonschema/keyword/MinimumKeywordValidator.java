/*
 * Copyright (c) 2012, Francis Galiegue <fgaliegue@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the Lesser GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Lesser GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.eel.kitchen.jsonschema.keyword;

import com.fasterxml.jackson.databind.JsonNode;
import org.eel.kitchen.jsonschema.main.ValidationReport;

import java.math.BigDecimal;

/**
 * Validator for the {@code minimum} keyword
 *
 * <p>This validator pairs with {@code exclusiveMinimum}. The latter has no
 * signification by itself without {@code minimum}.
 * </p>
 */
public final class MinimumKeywordValidator
    extends NumericKeywordValidator
{
    private final boolean exclusive;

    public MinimumKeywordValidator(final JsonNode schema)
    {
        super("minimum", schema);
        exclusive = schema.path("exclusiveMinimum").asBoolean(false);
    }

    @Override
    protected void validateLong(final ValidationReport report,
        final long instanceValue)
    {
        if (instanceValue > longValue)
            return;

        if (instanceValue < longValue) {
            report.addMessage("instance is lower than the required minimum");
            return;
        }

        if (exclusive)
            report.addMessage("instance is not strictly greater than the"
                + " required minimum");
    }

    @Override
    protected void validateDecimal(final ValidationReport report,
        final BigDecimal instanceValue)
    {
        final int cmp = instanceValue.compareTo(decimalValue);

        if (cmp > 0)
            return;

        if (cmp < 0) {
            report.addMessage("instance is lower than the required minimum");
            return;
        }

        if (exclusive)
            report.addMessage("instance is not strictly greater than the"
                + " required minimum");

    }
}
