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
import org.eel.kitchen.jsonschema.schema.JsonSchema;
import org.eel.kitchen.util.NodeType;

/**
 * Validator for the {@code disallow} keyword
 *
 * <p>{@code disallow} is the exact opposite of {@code type},
 * and if the JSON instance matches either a primitive type or a schema
 * within this keyword's definition, validation fails.</p>
 *
 * <p>FIXME: if a schema is a JSON reference and this reference does not
 * resolve successfully, then this particular schema fails validation,
 * and the logic is also inverted in that case. While the draft doesn't
 * object this, it can be viewed as a bug.</p>
 */
public final class DisallowKeywordValidator
    extends AbstractTypeKeywordValidator
{
    public DisallowKeywordValidator(final JsonNode schema)
    {
        super("disallow", schema);
    }

    @Override
    public void validate(final ValidationReport report,
        final JsonNode instance)
    {
        if (typeSet.contains(NodeType.getNodeType(instance))) {
            report.addMessage("instance is of a disallowed primitive type");
            return;
        }

        ValidationReport tmp;

        for (final JsonNode schema: schemas) {
            tmp = new ValidationReport();
            JsonSchema.fromNode(report.getSchema(), schema)
                .validate(tmp, instance);
            if (tmp.isSuccess()) {
                report.addMessage("instance matches a disallowed schema");
                return;
            }
        }
    }

}
