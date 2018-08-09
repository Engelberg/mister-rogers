/*
 * Copyright 2014 Ghent University, Bayer CropScience.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jamesframework.core.exceptions;

/**
 * Exception thrown when an incompatible search listener is attached to a search.
 * 
 * @author <a href="mailto:herman.debeukelaer@ugent.be">Herman De Beukelaer</a>
 */
public class IncompatibleSearchListenerException extends JamesRuntimeException {

    /**
     * Creates a new instance without detail message.
     */
    public IncompatibleSearchListenerException() {
    }

    /**
     * Constructs an instance with the specified detail message.
     * @param msg the detail message
     */
    public IncompatibleSearchListenerException(String msg) {
        super(msg);
    }
    
    /**
     * Constructs an instance with the specified cause.
     * @param cause other exception that caused this exception
     */
    public IncompatibleSearchListenerException(Throwable cause) {
        super(cause);
    }
    
    /**
     * Constructs an instance with the specified detail message and cause.
     * @param msg the detail message
     * @param cause other exception that caused this exception
     */
    public IncompatibleSearchListenerException(String msg, Throwable cause) {
        super(msg, cause);
    }
    
}
