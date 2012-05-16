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

import java.net.URI;
import java.net.URISyntaxException;

public final class URISyntaxChecker
    implements SyntaxChecker
{
    private final String keyword;

    public URISyntaxChecker(final String keyword)
    {
        this.keyword = keyword;
    }

    @Override
    public void checkValue(final ValidationReport report,
        final JsonNode schema)
    {
        final JsonNode node = schema.get(keyword);

        try {
            new URI(node.textValue());
        } catch (URISyntaxException ignored) {
            report.addMessage("not a valid URI");
        }
    }
}
