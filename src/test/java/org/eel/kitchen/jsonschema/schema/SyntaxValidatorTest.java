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

package org.eel.kitchen.jsonschema.schema;

import com.fasterxml.jackson.databind.JsonNode;
import org.eel.kitchen.jsonschema.main.ValidationReport;
import org.eel.kitchen.testutils.DataProviderArguments;
import org.eel.kitchen.testutils.JsonDataProvider;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public final class SyntaxValidatorTest
{
    @Test(
        dataProviderClass = JsonDataProvider.class,
        dataProvider = "getData"
    )
    @DataProviderArguments(fileName = "/syntax/syntax.json")
    public void testEntry(final JsonNode element)
    {
        final JsonNode schema = element.get("schema");
        final boolean valid = element.get("valid").booleanValue();

        final ValidationReport report = new ValidationReport();

        SyntaxValidator.validate(report, schema);

        assertEquals(report.isSuccess(), valid, schema.toString());
    }
}
