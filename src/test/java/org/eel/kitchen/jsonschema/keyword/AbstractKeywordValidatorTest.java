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
import org.eel.kitchen.util.JsonLoader;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import static org.testng.Assert.assertEquals;

public abstract class AbstractKeywordValidatorTest
{
    private final JsonNode testData;
    private final Constructor<? extends KeywordValidator> constructor;

    AbstractKeywordValidatorTest(final Class<? extends KeywordValidator> c,
        final String resourceName)
        throws IOException, NoSuchMethodException
    {
        final String input = "/keyword/" + resourceName + ".json";
        testData = JsonLoader.fromResource(input);

        constructor = c.getConstructor(JsonNode.class);
    }

    @DataProvider
    protected Iterator<Object[]> getData()
    {
        final Set<Object[]> set = new HashSet<Object[]>(testData.size());

        for (final JsonNode node: testData)
            set.add(mungeArguments(node));

        return set.iterator();
    }

    private Object[] mungeArguments(final JsonNode node)
    {
        return new Object[] {
            node.get("schema"),
            node.get("data"),
            node.get("valid").booleanValue()
        };
    }

    @Test(dataProvider = "getData")
    public final void testKeyword(final JsonNode schema, final JsonNode data,
        final boolean valid)
        throws InvocationTargetException, IllegalAccessException,
        InstantiationException
    {
        final KeywordValidator validator = constructor.newInstance(schema);

        final ValidationReport report = new ValidationReport();

        validator.validate(report, data);

        assertEquals(report.isSuccess(), valid);

    }
}
