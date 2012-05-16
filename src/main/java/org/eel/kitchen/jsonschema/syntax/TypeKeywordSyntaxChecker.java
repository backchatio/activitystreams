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

package org.eel.kitchen.jsonschema.syntax;

import com.fasterxml.jackson.databind.JsonNode;
import org.eel.kitchen.jsonschema.main.ValidationReport;
import org.eel.kitchen.util.NodeType;

public final class TypeKeywordSyntaxChecker
    implements SyntaxChecker
{
    private static final String ANY = "any";
    private final String keyword;

    public TypeKeywordSyntaxChecker(final String keyword)
    {
        this.keyword = keyword;
    }

    @Override
    public void checkValue(final ValidationReport report,
        final JsonNode schema)
    {
        final JsonNode node = schema.get(keyword);

        if (!node.isArray()) {
            validateOne(report, node);
            return;
        }

        for (final JsonNode value: node)
            validateOne(report, value);
    }

    private void validateOne(final ValidationReport report,
        final JsonNode value)
    {
        if (value.isObject())
            return;

        if (!value.isTextual()) {
            report.addMessage("value has wrong type");
            return;
        }

        final String s = value.textValue();

        if (ANY.equals(s))
            return;

        if (NodeType.fromName(s) == null)
            report.addMessage("unknown simple type");
    }
}
