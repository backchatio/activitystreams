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

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.eel.kitchen.jsonschema.main.JsonSchemaException;
import org.eel.kitchen.jsonschema.ref.JsonRef;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public final class JsonRefTest
{
    private final JsonNodeFactory factory = JsonNodeFactory.instance;

    @Test
    public void testNonObject()
        throws JsonSchemaException
    {
        JsonRef ref;

        ref = JsonRef.fromNode(factory.arrayNode(), "$ref");
        assertTrue(ref.isEmpty());

        ref = JsonRef.fromNode(factory.nullNode(), "$ref");
        assertTrue(ref.isEmpty());

        ref = JsonRef.fromNode(factory.numberNode(1), "$ref");
        assertTrue(ref.isEmpty());

        ref = JsonRef.fromNode(factory.textNode("foo"), "$ref");
        assertTrue(ref.isEmpty());

        ref = JsonRef.fromNode(factory.booleanNode(false), "$ref");
        assertTrue(ref.isEmpty());
    }

    @Test
    public void testNormalized()
        throws JsonSchemaException
    {
        final ObjectNode node1, node2;

        node1 = factory.objectNode();
        node1.put("$ref", "http://foo.bar/a/b");

        node2 = factory.objectNode();
        node2.put("$ref", "http://foo.bar/c/../a/./b");

        final JsonRef ref1 = JsonRef.fromNode(node1, "$ref");
        final JsonRef ref2 = JsonRef.fromNode(node2, "$ref");
        assertEquals(ref1, ref2);
    }

    @Test
    public void testEquals()
        throws JsonSchemaException
    {
        final ObjectNode node1, node2;

        node1 = factory.objectNode();
        node1.put("$ref", "http://foo.bar/a/b");

        node2 = factory.objectNode();
        node2.put("$ref", "http://foo.bar/c/../a/./b");

        final JsonRef ref1 = JsonRef.fromNode(node1, "$ref");
        final JsonRef ref2 = JsonRef.fromNode(node2, "$ref");

        assertFalse(ref1.equals(null));
        assertFalse(ref1.equals(""));
        assertTrue(ref1.equals(ref2));
        assertTrue(ref1.equals(ref1));
        assertTrue(ref2.equals(ref1));
    }

    @Test
    public void testAbsolute()
        throws JsonSchemaException
    {
        final ObjectNode node1, node2;

        node1 = factory.objectNode();
        node1.put("$ref", "http://foo.bar/a/b");

        node2 = factory.objectNode();
        node2.put("$ref", "foo.bar");

        final JsonRef ref1 = JsonRef.fromNode(node1, "$ref");
        final JsonRef ref2 = JsonRef.fromNode(node2, "$ref");

        assertTrue(ref1.isAbsolute());
        assertFalse(ref2.isAbsolute());
    }

    @Test
    public void testInvalidRefs()
    {
        final ObjectNode node = factory.objectNode();


        node.put("$ref", 1);

        try {
            JsonRef.fromNode(node, "$ref");
            fail("No exception thrown!");
        } catch (JsonSchemaException e) {
            assertEquals(e.getMessage(), "invalid $ref entry: not a string");
        }

        node.put("$ref", "+23:");

        try {
            JsonRef.fromNode(node, "$ref");
            fail("No exception thrown!");
        } catch (JsonSchemaException e) {
            assertEquals(e.getMessage(), "invalid $ref entry: not a valid URI");
        }
    }

    @Test
    public void testFragments()
        throws JsonSchemaException
    {
        final ObjectNode node = factory.objectNode();
        JsonRef ref;

        node.put("f1", "file:///a");
        node.put("f2", "file:///a#");
        node.put("f3", "file:///a#b/c");

        ref = JsonRef.fromNode(node, "f1");
        assertFalse(ref.hasFragment());

        ref = JsonRef.fromNode(node, "f2");
        assertFalse(ref.hasFragment());

        ref = JsonRef.fromNode(node, "f3");
        assertTrue(ref.hasFragment());
        assertEquals(ref.getFragment(), "b/c");
    }
}
